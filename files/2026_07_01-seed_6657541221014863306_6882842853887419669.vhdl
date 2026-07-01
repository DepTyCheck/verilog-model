-- Seed: 6657541221014863306,6882842853887419669

entity siekpecp is
  port (cs : buffer real);
end siekpecp;

architecture frmnmpfu of siekpecp is
  
begin
  -- Single-driven assignments
  cs <= 2#11.101#;
end frmnmpfu;

entity z is
  port (rh : inout time);
end z;

architecture b of z is
  signal f : real;
  signal rnlradra : real;
begin
  lbftllso : entity work.siekpecp
    port map (cs => rnlradra);
  ahqtfprtx : entity work.siekpecp
    port map (cs => f);
  
  -- Single-driven assignments
  rh <= 8#3_2_6_1_1.1# ns;
end b;



-- Seed after: 26986304201813661,6882842853887419669
