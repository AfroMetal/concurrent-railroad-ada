package body Trains is
    --type Route_Array is array (Integer range <>) of Integer;
    --type Route_Ptr is access Route_Array;

    procedure Connection(Self: Train_Ptr; From, To : out Track_Ptr) is
        From_Index : constant Integer := Self.Index;
        To_Index   : constant Integer := (Self.Index + 1) mod Self.Route'Length;
    begin
        From := Self.Route (From_Index+1);
        To := Self.Route (To_Index+1);
    end Connection;

    function Move_To(Self: Train_Ptr; T : Track_Ptr) return Float is
    begin
        Self.Att.Unlock;
        Self.Att := T;
        if T.Get_Type = Turntable then
            Self.Index := (Self.Index + 1) mod Self.Route'Length;
        end if;
        return Self.Att.Action_Time(Self.Speed);
    end Move_To;

    function As_String(Self: Train_Ptr) return String is
    begin
        return "Train" & Trim(Integer'Image (Self.Id), Left) & " " & SU.To_String (Self.Name);
    end As_String;

    function As_Verbose_String(Self: Train_Ptr) return String is
        Id    : String := Integer'Image (Self.Id);
        Name  : String := SU.To_String (Self.Name);
        Speed : String := Integer'Image (Self.Speed);
        Cap   : String := Integer'Image (Self.Capacity);
        Route : String := As_String(Self.Route);
        Att   : String := Self.Att.As_String;
    begin
        return "rails.Train:" & Name & ":" & Id (2 .. Id'Last) &
            "{speed:" & Speed (2 .. Speed'Last) & ", " &
            "cap:" & Cap (2 .. Cap'Last) & ", " &
            "route:" & Route & ", " &
            "at:" & Att & "}";
    end As_Verbose_String;

    function New_Train (
        Id       : Integer;
        Name     : SU.Unbounded_String;
        Speed    : Integer;
        Capacity : Integer;
        Route    : Route_Array
    ) return Train_Ptr is
        T : Train_Ptr;
    begin
        T := new Train(Route'Length);
        T.Id := Id;
        T.Name := Name;
        T.Speed := Speed;
        T.Capacity := Capacity;
        T.Route := Route;
        T.Index := 0;
        T.Att := Route (0);
        T.Att.Lock;
        return T;
    end New_Train;

    task body Simulation is
        Seconds_Per_Hour : Integer;
        Start            : Calendar.Time;
        Clock            : array(1..2) of Integer;
        Verbose          : Boolean;

        Train            : Train_Ptr;
        Connections      : Connections_Ptr;
        First            : Track_Ptr;
        Second           : Track_Ptr;
        Time             : Float;
        Current_Rails    : Tracks_Ptr;
        Success          : Boolean;

        Now              : String(1 .. 8);

        function Clock_Time return String is
            D : Duration;
            SH : Float;
            SM : Float;
            SS : Float;
            F : Float;
            H : Integer;
            M : Integer;
            S : Integer;
            H_String : SU.Unbounded_String;
            M_String : SU.Unbounded_String;
            S_String : SU.Unbounded_String;
        begin
            D := Calendar."-"(Calendar.Clock, Start);
            SH := Float (D) / Float (Seconds_Per_Hour);
            F := SH - Float'Floor (SH);
            SH := Float'Floor (SH);
            SM := 60.0 * F;
            F := SM - Float'Floor (SM);
            SS := 60.0 * F;

            H := Integer (SH) + Clock (1);
            H := H mod 24;
            M := Integer (SM) + Clock (2);
            if M > 59 then
                H := H + 1;
            end if;
            M := M mod 60;
            S := Integer (SS);

            H_String := SU.To_Unbounded_String (Trim(Integer'Image (H), Left));
            M_String := SU.To_Unbounded_String (Trim(Integer'Image (M), Left));
            S_String := SU.To_Unbounded_String (Trim(Integer'Image (S), Left));

            if SU.Length (H_String) = 1 then
                H_String := SU."&"('0', H_String);
            end if;

            if SU.Length (M_String) = 1 then
                M_String := SU."&"('0', M_String);
            end if;

            if SU.Length (S_String) = 1 then
                S_String := SU."&"('0', S_String);
            end if;

            return SU.To_String (H_String) & ":" & SU.To_String (M_String) & ":" & SU.To_String (S_String);
        end Clock_Time;

        procedure Print (S : String) is
        begin
            if Verbose then
                Put_Line (S);
            end if;
        end Print;
    begin
        accept Init(
            S   : Calendar.Time;
            SPH : Integer;
            H   : Integer;
            M   : Integer;
            V   : Boolean
        ) do
            Start := S;
            Seconds_Per_Hour := SPH;
            Clock (1) := H;
            Clock (2) := M;
            Verbose := V;
        end Init;

        accept Simulate(T : in Train_Ptr; C : in Connections_Ptr) do
            Train := T;
            Connections := C;
        end Simulate;

        while TRUE loop
            Connection(Train, First, Second);
            Current_Rails := Connections(First.Get_Id, Second.Get_Id);
        Loop1:
            loop
                for R in Current_Rails'Range loop
                    Current_Rails (R).Get_Lock(Success);
                    case Success is
                        when TRUE =>
                            Now := Clock_Time;
                            -- TODO: save to statistics if arriving at station
                            Time := Float (Seconds_Per_Hour) * Move_To(Train, Current_Rails (R));
                            Print(Now & " " & As_String(Train) & " travels along " & Current_Rails (R).As_String);
                            delay Duration(Time);
                            exit Loop1 when Success = TRUE;
                        when FALSE =>
                            goto Continue1;
                    end case;
                    <<Continue1>>
                end loop;
                --Time := Float (Seconds_Per_Hour) * 0.25;
                --Put_Line(Clock_Time & " " & As_String(Train) & " have nowhere to go, it will wait for " & Float'Image (Time) & "s");
                --delay Duration(Time);
            end loop Loop1;
        Loop2:
            loop
                Second.Get_Lock(Success);
                case Success is
                    when TRUE =>
                        Now := Clock_Time;
                        -- TODO: save to statistics if leaving station
                        Time := Float (Seconds_Per_Hour) * Move_To(Train, Second);
                        Print(Now & " " & As_String(Train) & " rotates at " & Second.As_String);
                        delay Duration(Time);
                        exit Loop2 when Success = TRUE;
                    when FALSE =>
                        --Time := Float (Seconds_Per_Hour) * 0.25;
                        --Put_Line(Clock_Time & " " & As_String(Train) & " have nowhere to go, it will wait for " & Float'Image (Time) & "s");
                        --delay Duration(Time);
                        goto Continue2;
                end case;
                <<Continue2>>
            end loop Loop2;
        end loop;

    end Simulation;
end Trains;
