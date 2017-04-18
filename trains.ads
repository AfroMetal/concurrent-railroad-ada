with Ada.Text_IO;                use Ada.Text_IO;

with Rails; use Rails;

package Trains is
    --type Route_Array is array (Integer range <>) of Integer;
    --type Route_Ptr is access Route_Array;

    type Train (Route_Length: Integer) is
    record
        Id : Integer;
        Name : SU.Unbounded_String;
        Speed : Integer;
        Capacity : Integer;
        Route: Route_Array(1 .. Route_Length);
        Index : Integer;
        Att : Track_Ptr;
    end record;

    type Train_Ptr is access Train;
    procedure Connection(Self: Train_Ptr; From, To : out Track_Ptr);
    function Move_To(Self: Train_Ptr; T : Track_Ptr) return Float;

    type Trains_Array is array(Integer range<>) of Train_Ptr;
    type Trains_Ptr is access Trains_Array;

    function New_Train (
        Id : Integer;
        Name : SU.Unbounded_String;
        Speed : Integer;
        Capacity: Integer;
        Route : Route_Array) return Train_Ptr;


    task type Simulation is
        entry Ride(T : in Train_Ptr; C : in Connections_Ptr; SPH : Integer);
    end Simulation;

    type Simulation_Ptr is access Simulation;
end Trains;
