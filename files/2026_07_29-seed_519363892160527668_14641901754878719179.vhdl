-- Seed: 519363892160527668,14641901754878719179

entity pvtnmd is
  port (ohsllvr : out character; f : out real; qakusivgv : in time; kwzv : in integer);
end pvtnmd;

architecture mfrz of pvtnmd is
  
begin
  -- Single-driven assignments
  f <= f;
  ohsllvr <= 'o';
end mfrz;

entity uakjxnlzrx is
  port (ggyeljc : out time);
end uakjxnlzrx;

architecture lmj of uakjxnlzrx is
  
begin
  -- Single-driven assignments
  ggyeljc <= ggyeljc;
end lmj;

entity xluwti is
  port (zn : buffer real_vector(3 downto 3));
end xluwti;

architecture qbkbt of xluwti is
  signal japk : integer;
  signal nhqcvq : real;
  signal t : character;
  signal ntzqwtvkrs : integer;
  signal ixdz : real;
  signal cxzwrcx : character;
  signal v : time;
begin
  eu : entity work.uakjxnlzrx
    port map (ggyeljc => v);
  ldj : entity work.pvtnmd
    port map (ohsllvr => cxzwrcx, f => ixdz, qakusivgv => v, kwzv => ntzqwtvkrs);
  ybcq : entity work.pvtnmd
    port map (ohsllvr => t, f => nhqcvq, qakusivgv => v, kwzv => japk);
  
  -- Single-driven assignments
  zn <= zn;
  ntzqwtvkrs <= ntzqwtvkrs;
  japk <= ntzqwtvkrs;
end qbkbt;



-- Seed after: 13681733872091011702,14641901754878719179
