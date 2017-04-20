--
-- Radoslaw Kowalski 221454
--
with Ada.Text_IO;           use Ada.Text_IO;
with GNAT.OS_Lib;           use GNAT.OS_Lib;
with Ada.Command_line;      use Ada.Command_Line;
with Ada.Calendar;
with Ada.Exceptions;        use Ada.Exceptions;
with GNAT.String_Split;     use GNAT.String_Split;
with Ada.Strings.Unbounded;
with Trains;                use Trains;
with Rails;                 use Rails;

procedure Main is
    package SU renames Ada.Strings.Unbounded;
    package Calendar renames Ada.Calendar;

    type Argument_Option is (verbose, input, output);
    type Boolean_Flag is (True, False);

    File             : File_Type;
    Fields           : Slice_Set;
    Seconds_Per_Hour : Integer;
    Start            : Calendar.Time;
    Clock            : array(1..2) of Integer;
    Verbose_Flag     : Boolean := FALSE;
    In_Filename      : SU.Unbounded_String := SU.To_Unbounded_String ("input");
    Out_Filename     : SU.Unbounded_String := SU.To_Unbounded_String ("output");
    T                : Integer; -- number of trains
    TT               : Integer; -- number of Turntables
    NT               : Integer; -- number of NormalTracks
    ST               : Integer; -- number of StationTracks
    Turntables       : Turntables_Ptr;
    Normal_Tracks    : Normal_Tracks_Ptr;
    Station_Tracks   : Station_Tracks_Ptr;
    Trains           : Trains_Ptr;
    Connections      : Connections_Ptr;

    type Command is ('c', 'p', 't', 'u', 'n', 's', 'h', 'q');

    task Talk is
        entry Start;
    end Talk;

    task body Talk is
        C : String (1..1);
        Last : Natural;
    begin
        accept Start;

        Put_Line("Input char for action, availble commands:");
		Put_Line("    'p' - current trains positions,");
		Put_Line("    't' - list trains,");
        Put_Line("    'u' - list turntables,");
        Put_Line("    'n' - list normal tracks,");
		Put_Line("    's' - list station tracks,");
		Put_Line("    'h' - print this menu again,");
		Put_Line("    'q' - to quit simulation.");

        while TRUE loop

            Get_Line(C, Last);

            case C(1) is
                when 'p' =>
                null;
                    for I in 0 .. T-1 loop
                        Put_Line(As_String(Trains (I)) & ": " & Trains (I).Att.As_String);
                    end loop;
                when 't' =>
                    for I in 0 .. T-1 loop
                        Put_Line(As_Verbose_String(Trains (I)));
                    end loop;
                when 'u' =>
                    for I in 0 .. TT-1 loop
                        Put_Line(Turntables (I).As_Verbose_String);
                    end loop;
                when 'n' =>
                    for I in 0 .. NT-1 loop
                        Put_Line(Normal_Tracks (I).As_Verbose_String);
                    end loop;
                when 's' =>
                    for I in 0 .. ST-1 loop
                        Put_Line(Station_Tracks (I).As_Verbose_String);
                    end loop;
                when 'h' =>
                    Put_Line("Input char for action, availble commands:");
                    Put_Line("    'p' - current trains positions,");
                    Put_Line("    't' - list trains,");
                    Put_Line("    'u' - list turntables,");
                    Put_Line("    'n' - list normal tracks,");
                    Put_Line("    's' - list station tracks,");
                    Put_Line("    'h' - print this menu again,");
                    Put_Line("    'q' - to quit simulation.");
                when 'q' => OS_Exit(0);
                when others => null;
            end case;

            C := "#";

        end loop;
    end Talk;

    function Read_Fields(F: File_Type; Expected: Positive) return Slice_Set is
        File_Line        : SU.Unbounded_String;
        Fields           : Slice_Set;
        Seps             : constant String := " ";
        Fields_Exception : exception;
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
    end Read_Fields;

begin

    for Arg in 1 .. Argument_Count/2 loop
        declare
            A : Argument_Option;
        begin
            A := Argument_Option'Value (Argument(Arg*2-1));
            case A is
                when Argument_Option'(Verbose) =>
                    case Boolean_Flag'Value (Argument(Arg*2)) is
                        when Boolean_Flag'(true) =>
                            Verbose_Flag := TRUE;
                            Put_Line ("Verbose mode");
                        when others =>
                            Verbose_Flag := FALSE;
                            Put_Line ("Silent mode");
                    end case;
                when Input =>
                    In_Filename := SU.To_Unbounded_String (Argument(Arg*2));
                when Output =>
                    Out_Filename := SU.To_Unbounded_String (Argument(Arg*2));
                when others =>
                    null;
            end case;
        exception
            when Constraint_Error =>
                Put_Line ("Wrong arguments");
                OS_Exit(1);
        end;
    end loop;

    Open (File => File,
        Mode => In_File,
        Name => SU.To_String (In_Filename));

    Fields := Read_Fields(File, 1);
    Seconds_Per_Hour := Integer'Value (Slice (Fields, 1));

    Fields := Read_Fields(File, 2);
    Clock (1) := Integer'Value (Slice (Fields, 1));
    Clock (2) := Integer'Value (Slice (Fields, 2));

    Fields := Read_Fields(File, 4);
    T := Integer'Value (Slice (Fields, 1));
    TT := Integer'Value (Slice (Fields, 2));
    NT := Integer'Value (Slice (Fields, 3));
    ST := Integer'Value (Slice (Fields, 4));

    Put_Line (Integer'Image (T) & " Trains");
    Put_Line (Integer'Image (TT) & " turntables");
    Put_Line (Integer'Image (NT) & " normal tracks");
    Put_Line (Integer'Image (ST) & " station tracks");
    Put_Line ("Hour simulation will take" & Integer'Image (Seconds_Per_Hour) & " seconds");
    Put_Line ("Simulation starts at" & Integer'Image (Clock(1)) & ":" & Integer'Image (Clock(2)));

    Connections := new Connections_Array(0..TT-1, 0..TT-1);
    for I in 0 .. TT-1 loop
        for J in 0 .. TT-1 loop
            Connections(I, J) := new Tracks_Array(1 .. 0);
        end loop;
    end loop;

    Turntables := new Turntables_Array(0 .. TT-1);
    for I in 0 .. TT-1 loop
        declare
            Id : Integer;
            Time : Integer;
        begin
            Fields := Read_Fields(File, 2);
            Id := Integer'Value (Slice (Fields, 1));
            Time := Integer'Value (Slice (Fields, 2));

            Turntables (I) := New_Turntable(Id, Time);
        end;
    end loop;

    Normal_Tracks := new Normal_Tracks_Array(0 .. NT-1);
    for I in 0 .. NT-1 loop
        declare
            Id     : Integer;
            Length : Integer;
            Speed  : Integer;
            First  : Integer;
            Second : Integer;
        begin
            Fields := Read_Fields(File, 5);
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

    Station_Tracks := new Station_Tracks_Array(0 .. ST-1);
    for I in 0 .. ST-1 loop
        declare
            Id     : Integer;
            Name   : SU.Unbounded_String;
            Time   : Integer;
            First  : Integer;
            Second : Integer;
        begin
            Fields := Read_Fields(File, 5);
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

    Trains := new Trains_Array(0 .. T-1);
    for I in 0 .. T-1 loop
        declare
            Id           : Integer;
            Speed        : Integer;
            Capacity     : Integer;
            Name         : SU.Unbounded_String;
            Route_Length : Integer;
            Route        : Route_Ptr;
        begin
            Fields := Read_Fields(File, 5);
            Id := Integer'Value (Slice (Fields, 1));
            Speed := Integer'Value (Slice (Fields, 2));
            Capacity := Integer'Value (Slice (Fields, 3));
            Name := SU.To_Unbounded_String (Slice (Fields, 4));
            Route_Length := Integer'Value (Slice (Fields, 5));
            Route := new Route_Array(0 .. Route_Length-1);

            Fields := Read_Fields(File, Route_Length);
            for I in 0 .. Route_Length-1 loop
                declare
                    Index : Integer;
                begin
                    Index := Integer'Value (Slice (Fields, Slice_Number (I+1)));
                    Route (I) := Turntables (Index);
                end;
            end loop;

            Trains (I) := New_Train(Id, Name, Speed, Capacity, Route.all);
        end;
    end loop;

    Start := Calendar.Clock;
    for I in 0 .. T-1 loop
        declare
            Sim : Simulation_Ptr;
        begin
            Sim := new Simulation;
            Sim.Init(Start, Seconds_Per_Hour, Clock (1), Clock (2), Verbose_Flag);
            Sim.Simulate(Trains (I), Connections);
        end;
    end loop;

    if not Verbose_Flag then
        Talk.Start;
    end if;

    Close (File);
end Main;
