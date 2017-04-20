with Ada.Strings.Unbounded;

package Rails is
    package SU renames Ada.Strings.Unbounded;

    type Track_Type is (Turntable, Normal, Station);

    type Track_Record (Typee: Track_Type := Turntable) is
    record
        case Typee is
            when Turntable =>
                Rotation_Time : Integer;
            when Normal =>
                Length : Integer;
                Speed_Limit : Integer;
            when Station =>
                Stop_Time : Integer;
                Name : SU.Unbounded_String;
        end case;
    end record;

    protected type Track (T : Track_Type) is
        function Get_Id return Integer;
        function Get_Type return Track_Type;
        entry Get_Lock(Suc : out Boolean);
        entry Lock;
        entry Unlock;
        function As_String return String;
        function As_Verbose_String return String;
        function Action_Time(Train_Speed : Integer) return Float;
        procedure Init (I : in Integer; S : in Track_Record);
    private
        Id : Integer;
        Locked : Boolean := FALSE;
        Typee : Track_Type := T;
        Spec : Track_Record (T);
    end Track;

    type Track_Ptr is access Track;

    type Tracks_Array is array(Integer range <>) of Track_Ptr;
    type Tracks_Ptr is access Tracks_Array;

    type Route_Array is new Tracks_Array;
    function As_String(Self: Route_Array) return String;

    type Route_Ptr is access Route_Array;

    function New_Turntable(I : Integer; T : Integer) return Track_Ptr;

    function New_Normal_Track(I : Integer; L : Integer; SL : Integer) return Track_Ptr;

    function New_Station_Track(I : Integer; T : Integer; N : SU.Unbounded_String) return Track_Ptr;

    type Turntables_Array is new Tracks_Array;
    type Turntables_Ptr is access Turntables_Array;

    type Normal_Tracks_Array is new Tracks_Array;
    type Normal_Tracks_Ptr is access Normal_Tracks_Array;

    type Station_Tracks_Array is new Tracks_Array;
    type Station_Tracks_Ptr is access Station_Tracks_Array;

    type Connections_Array is array(Integer range <>, Integer range <>) of Tracks_Ptr;

    type Connections_Ptr is access Connections_Array;

end Rails;
