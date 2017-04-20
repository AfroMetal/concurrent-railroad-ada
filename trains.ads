with Ada.Text_IO;       use Ada.Text_IO;
with Ada.Strings;       use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Calendar;

with Rails;             use Rails;

package Trains is
    package Calendar renames Ada.Calendar;

    type Train (Route_Length: Integer) is
    record
        Id       : Integer;
        Name     : SU.Unbounded_String;
        Speed    : Integer;
        Capacity : Integer;
        Route    : Route_Array(1 .. Route_Length);
        Index    : Integer;
        Att      : Track_Ptr;
    end record;

    type Train_Ptr is access Train;
    procedure Connection(Self: Train_Ptr; From, To : out Track_Ptr);
    function Move_To(Self: Train_Ptr; T : Track_Ptr) return Float;
    function As_String(Self: Train_Ptr) return String;
    function As_Verbose_String(Self: Train_Ptr) return String;

    type Trains_Array is array(Integer range<>) of Train_Ptr;
    type Trains_Ptr is access Trains_Array;

    function New_Train (
        Id       : Integer;
        Name     : SU.Unbounded_String;
        Speed    : Integer;
        Capacity : Integer;
        Route    : Route_Array) return Train_Ptr;

    task type Simulation is
        entry Init(S : Calendar.Time; SPH : Integer; H : Integer; M : Integer; V : Boolean);
        entry Simulate(T : in Train_Ptr; C : in Connections_Ptr);
    end Simulation;

    type Simulation_Ptr is access Simulation;

    type Time_Component_Type is
         delta 1.0 digits 2 range 0.0 .. 60.0;
end Trains;
