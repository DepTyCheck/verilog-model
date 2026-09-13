-- Seed: 18431686130778915973,10754487200446211253

entity qiqrfzzhm is
  port (z : inout integer; jejz : linkage time; uahmksida : out integer);
end qiqrfzzhm;

architecture x of qiqrfzzhm is
  
begin
  
end x;

entity yiexhuti is
  port (bij : buffer boolean_vector(4 to 3));
end yiexhuti;

architecture gp of yiexhuti is
  signal g : integer;
  signal khxhy : time;
  signal rjajl : integer;
  signal ttuxzcfk : integer;
  signal mhxmcqretv : time;
  signal suyzcp : integer;
begin
  l : entity work.qiqrfzzhm
    port map (z => suyzcp, jejz => mhxmcqretv, uahmksida => ttuxzcfk);
  wqss : entity work.qiqrfzzhm
    port map (z => rjajl, jejz => khxhy, uahmksida => g);
  
  -- Single-driven assignments
  bij <= bij;
end gp;

entity oxkdgre is
  port (touiujo : in real_vector(1 to 2); aftsjk : in integer_vector(0 to 4));
end oxkdgre;

architecture y of oxkdgre is
  signal w : integer;
  signal ihednpf : time;
  signal ybkpqzxz : integer;
  signal lemgxhijzc : integer;
  signal ppdizvpy : time;
  signal yprlptit : integer;
  signal kwxadglh : boolean_vector(4 to 3);
  signal xhse : integer;
  signal qpbheuxhu : time;
  signal rcokka : integer;
begin
  b : entity work.qiqrfzzhm
    port map (z => rcokka, jejz => qpbheuxhu, uahmksida => xhse);
  vf : entity work.yiexhuti
    port map (bij => kwxadglh);
  loxekxqs : entity work.qiqrfzzhm
    port map (z => yprlptit, jejz => ppdizvpy, uahmksida => lemgxhijzc);
  btoaieylez : entity work.qiqrfzzhm
    port map (z => ybkpqzxz, jejz => ihednpf, uahmksida => w);
end y;

library ieee;
use ieee.std_logic_1164.all;

entity iokturh is
  port (kgwndp : buffer std_logic_vector(2 to 2));
end iokturh;

architecture ggt of iokturh is
  signal yfnwms : integer;
  signal haxozznk : time;
  signal f : integer;
  signal tfzvn : integer_vector(0 to 4);
  signal sfhbfqm : real_vector(1 to 2);
begin
  tgsupaxl : entity work.oxkdgre
    port map (touiujo => sfhbfqm, aftsjk => tfzvn);
  ryjznbaxvs : entity work.qiqrfzzhm
    port map (z => f, jejz => haxozznk, uahmksida => yfnwms);
  
  -- Single-driven assignments
  sfhbfqm <= (16#BFE.C_3_D#, 02023.1);
  tfzvn <= (2#00100#, 2, 8#67#, 16#DDC7#, 2#1#);
  
  -- Multi-driven assignments
  kgwndp <= kgwndp;
  kgwndp <= (others => 'U');
  kgwndp <= "1";
  kgwndp <= "-";
end ggt;



-- Seed after: 14661035516907425067,10754487200446211253
