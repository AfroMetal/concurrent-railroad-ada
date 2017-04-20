package body Rails is

    protected body Track is
        function Get_Id return Integer is
        begin
            return Id;
        end Get_Id;

        function Get_Type return Track_Type is
        begin
            return Typee;
        end Get_Type;

        entry Get_Lock(Suc : out Boolean) when TRUE is
        begin
            case Locked is
                when TRUE =>
                    Suc := FALSE;
                when FALSE =>
                    Locked := TRUE;
                    Suc := TRUE;
            end case;
        end Get_Lock;

        entry Lock when not Locked is
        begin
            Locked := TRUE;
        end Lock;

        entry Unlock when Locked is
        begin
            Locked := FALSE;
        end Unlock;

        function As_String return String is
        begin
            case Typee is
                when Turntable =>
                    return "Turntable" & Integer'Image (Id);
                when Normal =>
                    return "NormalTrack" & Integer'Image (Id);
                when Station =>
                    return "StationTrack" & Integer'Image (Id) & " " & SU.To_String (Spec.Name);
            end case;
        end As_String;

        function As_Verbose_String return String is
            Id : String := Integer'Image (Get_Id);
        begin
            case Typee is
                when Turntable =>
                    return "rails.Turntable:" & Id (2 .. Id'Last) & "{time:" &
                    Integer'Image (Spec.Rotation_Time) & "}";
                when Normal =>
                    return "rails.NormalTrack:" & Id (2 .. Id'Last) & "{len:" &
                    Integer'Image (Spec.Length) & ", limit:" & Integer'Image (Spec.Speed_Limit) & "}";
                when Station =>
                    return "rails.StationTrack:" & Id (2 .. Id'Last) & ":" & SU.To_String (Spec.Name) & "{time:" &
                    Integer'Image (Spec.Stop_Time) & "}";
            end case;
        end As_Verbose_String;

        function Action_Time(Train_Speed : Integer) return Float is
        begin
            case Typee is
                when Turntable =>
                    return Float (Spec.Rotation_Time) / 60.0;
                when Normal =>
                    return Float (Spec.Length) / Float (Integer'Min(Spec.Speed_Limit, Train_Speed));
                when Station =>
                    return Float (Spec.Stop_Time) / 60.0;
            end case;
        end Action_Time;

        procedure Init (I : in Integer; S : in Track_Record) is
        begin
            Id := I;
            Spec := S;
        end Init;
    end Track;

    function New_Turntable(I : Integer; T : Integer) return Track_Ptr is
        TP : Track_Ptr;
        S  : Track_Record;
    begin
        TP := new Track(Turntable);
        S :=  (Typee => Turntable, Rotation_Time => T);
        TP.Init(I, S);
        return TP;
    end New_Turntable;

    function New_Normal_Track(I : Integer; L : Integer; SL : Integer) return Track_Ptr is
        TP : Track_Ptr;
        S  : Track_Record;
    begin
        TP := new Track(Normal);
        S :=  (Typee => Normal, Length => L, Speed_Limit => SL);
        TP.Init(I, S);
        return TP;
    end New_Normal_Track;

    function New_Station_Track(I : Integer; T : Integer; N : SU.Unbounded_String) return Track_Ptr is
        TP : Track_Ptr;
        S  : Track_Record;
    begin
        TP := new Track(Station);
        S :=  (Typee => Station, Stop_Time => T, Name => N);
        TP.Init(I, S);
        return TP;
    end New_Station_Track;


    function As_String(Self: Route_Array) return String is
        S : SU.Unbounded_String;
    begin
        S := SU.To_Unbounded_String ("[");
        for I in Self'range loop
                SU.Append(S, Integer'Image (I) (2 .. Integer'Image (I)'Last) & " ");
        end loop;
        SU.Append(S, "]");

        return SU.To_String (S);
    end As_String;
end Rails;
