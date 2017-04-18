package body Trains is
    --type Route_Array is array (Integer range <>) of Integer;
    --type Route_Ptr is access Route_Array;

    procedure Connection(Self: Train_Ptr; From, To : out Track_Ptr) is
        From_Index : constant Integer := Self.Index;
        To_Index : constant Integer := (Self.Index + 1) mod Self.Route'Length;
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

    function New_Train (
    Id : Integer;
    Name : SU.Unbounded_String;
    Speed : Integer;
    Capacity: Integer;
    Route : Route_Array) return Train_Ptr is
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
        Train : Train_Ptr;
        Connections : Connections_Ptr;
        Seconds_Per_Hour : Integer;
        First : Track_Ptr;
        Second : Track_Ptr;
        Time : Float;
        Current_Rails : Tracks_Ptr;
        Success : Boolean;
    begin
        accept Ride(T : in Train_Ptr; C : in Connections_Ptr; SPH : Integer) do
            Train := T;
            Connections := C;
            Seconds_Per_Hour := SPH;
        end Ride;

        while TRUE loop
            Connection(Train, First, Second);
            Current_Rails := Connections(First.Get_Id, Second.Get_Id);
        Loop1:
            loop
                for R in Current_Rails'Range loop
                    Current_Rails (R).Get_Lock(Success);
                    case Success is
                        when TRUE =>
                            Time := Float (Seconds_Per_Hour) * Move_To(Train, Current_Rails (R));
                            Put_Line("Train" & Integer'Image (Train.Id) & " travels along " & Current_Rails (R).As_String);
                            delay Duration(Time);
                            exit Loop1 when Success = TRUE;
                        when FALSE =>
                            goto Continue1;
                    end case;
                    <<Continue1>>
                end loop;
                Time := Float (Seconds_Per_Hour) * 0.25;
                Put_Line("Train" & Integer'Image (Train.Id) & " have nowhere to go, it will wait for " & Float'Image (Time) & "s");
                delay Duration(Time);
            end loop Loop1;
        Loop2:
            loop
                Second.Get_Lock(Success);
                case Success is
                    when TRUE =>
                        Time := Float (Seconds_Per_Hour) * Move_To(Train, Second);
                        Put_Line("Train" & Integer'Image (Train.Id) & " rotates at " & Second.As_String);
                        delay Duration(Time);
                        exit Loop2 when Success = TRUE;
                    when FALSE =>
                        Time := Float (Seconds_Per_Hour) * 0.25;
                        Put_Line("Train" & Integer'Image (Train.Id) & " have nowhere to go, it will wait for " & Float'Image (Time) & "s");
                        delay Duration(Time);
                        goto Continue2;
                end case;
                <<Continue2>>
            end loop Loop2;
        end loop;

    end Simulation;
end Trains;
