-- Seed: 6988070750790856340,8421704836678237495

entity ovpt is
  port (iqguvb : buffer bit; frqdiygz : in boolean; wjmx : linkage real; drfnfaio : linkage time);
end ovpt;

architecture fan of ovpt is
  
begin
  -- Single-driven assignments
  iqguvb <= '1';
end fan;

entity uuev is
  port (tmhjbp : buffer time_vector(1 to 4); xhzsb : linkage bit; cclx : inout boolean);
end uuev;

architecture mxyorbti of uuev is
  signal gex : time;
  signal cbebl : real;
  signal pidd : bit;
begin
  zdmx : entity work.ovpt
    port map (iqguvb => pidd, frqdiygz => cclx, wjmx => cbebl, drfnfaio => gex);
  
  -- Single-driven assignments
  tmhjbp <= (3 sec, 2#0_0_0# us, 2#1_1_0_1# ns, 0 min);
  cclx <= TRUE;
end mxyorbti;



-- Seed after: 957250169735040320,8421704836678237495
