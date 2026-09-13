-- Seed: 3227504912946796400,10754487200446211253

entity po is
  port (blvayrl : buffer real; kfgp : inout integer; n : in time);
end po;

architecture iiwuja of po is
  
begin
  -- Single-driven assignments
  kfgp <= 2#0_1_0_1#;
  blvayrl <= blvayrl;
end iiwuja;

entity vovmwyf is
  port (rz : buffer real);
end vovmwyf;

architecture pcea of vovmwyf is
  signal svh : time;
  signal yhom : integer;
  signal onjkia : integer;
  signal nrvdlwpe : real;
  signal oogock : time;
  signal cpx : integer;
  signal auilkva : real;
begin
  wqzhvvvt : entity work.po
    port map (blvayrl => auilkva, kfgp => cpx, n => oogock);
  uduk : entity work.po
    port map (blvayrl => nrvdlwpe, kfgp => onjkia, n => oogock);
  xalxh : entity work.po
    port map (blvayrl => rz, kfgp => yhom, n => svh);
  
  -- Single-driven assignments
  oogock <= 3_2_2_0.1_2_2 fs;
  svh <= 3 sec;
end pcea;



-- Seed after: 5757782234231451145,10754487200446211253
