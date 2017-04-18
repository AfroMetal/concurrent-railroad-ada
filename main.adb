with Ada.Text_IO;                use Ada.Text_IO;
with Ada.Exceptions;             use Ada.Exceptions;
with GNAT.String_Split;          use GNAT.String_Split;
with Ada.Strings.Unbounded;
with Trains;                     use Trains;
with Rails;                      use Rails;

procedure Main is
    package SU renames Ada.Strings.Unbounded;
    File : File_Type;
    Fields : Slice_Set;
    SecondsPerHour : Integer;
    Clock : array(1..2) of Integer;
    T : Integer; -- number of trains
    TT : Integer; -- number of Turntables
    NT : Integer; -- number of NormalTracks
    ST : Integer; -- number of StationTracks
    Turntables : Turntables_Ptr;
    Normal_Tracks : Normal_Tracks_Ptr;
    Station_Tracks : Station_Tracks_Ptr;
    Trains : Trains_Ptr;
    Connections : Connections_Ptr;

    function ReadFields(F: File_Type; Expected: Positive) return Slice_Set is
        File_Line: SU.Unbounded_String;
        Fields: Slice_Set;
        Seps: constant String := " ";
        Fields_Exception: exception;
    begin
        File_Line := SU.To_Unbounded_String (Get_Line (F));

        loop
            exit when SU.To_String (File_Line)'Length > 0 and then SU.To_String (File_Line) (1) /= '#';

            File_Line := SU.To_Unbounded_String (Get_Line (F));
        end loop;

        Create (S => Fields,
                     From => SU.To_String (File_Line),
                     Separators => Seps,
                     Mode => Multiple);

        if Positive (Slice_Count (Fields)) /= Expected then
            Raise_Exception (Fields_Exception'Identity, "Expected to read" & Positive'Image (Expected) & " fields");
        end if;

        return Fields;
    exception
        when Fail: Fields_Exception =>
            Put_Line (Exception_Message (Fail));
        return Fields;
    end ReadFields;

begin
    Open (File => File,
        Mode => In_File,
        Name => "input");

    Fields := ReadFields(File, 1);
    SecondsPerHour := Integer'Value (Slice (Fields, 1));

    Fields := ReadFields(File, 2);
    Clock (1) := Integer'Value (Slice (Fields, 1));
    Clock (2) := Integer'Value (Slice (Fields, 2));

    Fields := ReadFields(File, 4);
    T := Integer'Value (Slice (Fields, 1));
    TT := Integer'Value (Slice (Fields, 2));
    NT := Integer'Value (Slice (Fields, 3));
    ST := Integer'Value (Slice (Fields, 4));

    Turntables := new Turntables_Array(0 .. TT-1);
    Normal_Tracks := new Normal_Tracks_Array(0 .. NT-1);
    Station_Tracks := new Station_Tracks_Array(0 .. ST-1);

    Trains := new Trains_Array(0 .. T-1);

    Connections := new Connections_Array(0..TT-1, 0..TT-1);
    for I in 0 .. TT-1 loop
        for J in 0 .. TT-1 loop
            Connections(I, J) := new Tracks_Array(1 .. 0);
        end loop;
    end loop;

    Put_Line (Integer'Image (T) & " Trains");
    Put_Line (Integer'Image(TT) & " turntables");
    Put_Line (Integer'Image (NT) & " normal tracks");
    Put_Line (Integer'Image (ST) & " station tracks");
    Put_Line ("Hour simulation will take" & Integer'Image (SecondsPerHour) & " seconds");
    Put_Line ("Simulation starts at" & Integer'Image (Clock(1)) & ":" & Integer'Image (Clock(2)));

    for I in 0 .. TT-1 loop
        declare
            Id : Integer;
            Time : Integer;
        begin
            Fields := ReadFields(File, 2);
            Id := Integer'Value (Slice (Fields, 1));
            Time := Integer'Value (Slice (Fields, 2));

            Turntables (I) := New_Turntable(Id, Time);
        end;
    end loop;

    for I in 0 .. NT-1 loop
        declare
            Id : Integer;
            Length : Integer;
            Speed : Integer;
            First : Integer;
            Second : Integer;
        begin
            Fields := ReadFields(File, 5);
            Id := Integer'Value (Slice (Fields, 1));
            Length := Integer'Value (Slice (Fields, 2));
            Speed := Integer'Value (Slice (Fields, 3));
            First := Integer'Value (Slice (Fields, 4));
            Second := Integer'Value (Slice (Fields, 5));

            Normal_Tracks (I) := New_Normal_Track(Id, Length, Speed);

            if Connections (First, Second)'Length = 0 then
                Connections (First, Second) := new Tracks_Array(0 .. 0);
                Connections (First, Second) (0) := Normal_Tracks (I);
            else
                Connections (First, Second) := new Tracks_Array'(Connections (First, Second).all & Normal_Tracks (I));
            end if;

            if Connections (Second, First)'Length = 0 then
                Connections (Second, First) := new Tracks_Array(0 .. 0);
                Connections (Second, First) (0) := Normal_Tracks (I);
            else
                Connections (First, First) := new Tracks_Array'(Connections (Second, First).all & Normal_Tracks (I));
            end if;
        end;
    end loop;

    for I in 0 .. ST-1 loop
        declare
            Id : Integer;
            Name : SU.Unbounded_String;
            Time : Integer;
            First : Integer;
            Second : Integer;
        begin
            Fields := ReadFields(File, 5);
            Id := Integer'Value (Slice (Fields, 1));
            Name := SU.To_Unbounded_String (Slice (Fields, 2));
            Time := Integer'Value (Slice (Fields, 3));
            First := Integer'Value (Slice (Fields, 4));
            Second := Integer'Value (Slice (Fields, 5));

            Station_Tracks (I) := New_Station_Track(Id, Time, Name);

            if Connections (First, Second)'Length = 0 then
                Connections (First, Second) := new Tracks_Array(0 .. 0);
                Connections (First, Second) (0) := Station_Tracks (I);
            else
                Connections (First, Second) := new Tracks_Array'(Connections (First, Second).all & Station_Tracks (I));
            end if;

            if Connections (Second, First)'Length = 0 then
                Connections (Second, First) := new Tracks_Array(0 .. 0);
                Connections (Second, First) (0) := Station_Tracks (I);
            else
                Connections (First, First) := new Tracks_Array'(Connections (Second, First).all & Station_Tracks (I));
            end if;
        end;
    end loop;

    for I in 0 .. T-1 loop
        declare
            Id : Integer;
            Speed : Integer;
            Capacity : Integer;
            Name : SU.Unbounded_String;
            Route_Length : Integer;
            Route : Route_Ptr;
            Sim : Simulation_Ptr;
        begin
            Fields := ReadFields(File, 5);
            Id := Integer'Value (Slice (Fields, 1));
            Speed := Integer'Value (Slice (Fields, 2));
            Capacity := Integer'Value (Slice (Fields, 3));
            Name := SU.To_Unbounded_String (Slice (Fields, 4));
            Route_Length := Integer'Value (Slice (Fields, 5));
            Route := new Route_Array(0 .. Route_Length-1);

            Fields := ReadFields(File, Route_Length);
            for I in 0 .. Route_Length-1 loop
                declare
                    Index : Integer;
                begin
                    Index := Integer'Value (Slice (Fields, Slice_Number (I+1)));
                    Route (I) := Turntables (Index);
                end;
            end loop;

            Trains (I) := New_Train(Id, Name, Speed, Capacity, Route.all);

            -- TODO: start train
            Sim := new Simulation;
            Sim.Ride(Trains (I), Connections, SecondsPerHour);
        end;
    end loop;

    Close (File);
end Main;
