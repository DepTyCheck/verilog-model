-- Seed: 3928104359951615378,14426950258250697445

entity hcsapm is
  port (nkhv : out time; hhrmyiv : inout time);
end hcsapm;

architecture tfls of hcsapm is
  
begin
  -- Single-driven assignments
  nkhv <= hhrmyiv;
  hhrmyiv <= 3 sec;
end tfls;



-- Seed after: 14450234148720956420,14426950258250697445
