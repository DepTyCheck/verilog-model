-- Seed: 16313500348761675749,6697892553037813751

entity mh is
  port (pdsdzw : in time; yxb : inout bit);
end mh;

architecture vky of mh is
  
begin
  -- Single-driven assignments
  yxb <= '0';
end vky;

entity mmjiffkqu is
  port (hovyrhsrq : out bit);
end mmjiffkqu;

architecture dvoxheqls of mmjiffkqu is
  signal apr : bit;
  signal xaqjeaoxd : time;
begin
  jtnj : entity work.mh
    port map (pdsdzw => xaqjeaoxd, yxb => hovyrhsrq);
  gefz : entity work.mh
    port map (pdsdzw => xaqjeaoxd, yxb => apr);
  
  -- Single-driven assignments
  xaqjeaoxd <= 0_0_1_1.3 fs;
end dvoxheqls;



-- Seed after: 9995065088724083043,6697892553037813751
