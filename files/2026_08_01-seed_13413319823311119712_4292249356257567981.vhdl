-- Seed: 13413319823311119712,4292249356257567981

library ieee;
use ieee.std_logic_1164.all;

entity m is
  port (ffgjntbh : linkage std_logic_vector(4 downto 3); rsijux : buffer std_logic; vgrgkxu : linkage boolean_vector(4 downto 3));
end m;

architecture kfpxgjwg of m is
  
begin
  -- Multi-driven assignments
  rsijux <= '1';
  rsijux <= rsijux;
  rsijux <= rsijux;
end kfpxgjwg;

library ieee;
use ieee.std_logic_1164.all;

entity cqoijsbu is
  port (kgqzxbfpjw : linkage std_logic);
end cqoijsbu;

library ieee;
use ieee.std_logic_1164.all;

architecture vhsegwnj of cqoijsbu is
  signal cfghbauue : boolean_vector(4 downto 3);
  signal obg : boolean_vector(4 downto 3);
  signal woropnnl : std_logic;
  signal ebvdz : boolean_vector(4 downto 3);
  signal z : std_logic;
  signal dicy : boolean_vector(4 downto 3);
  signal dycmttlj : std_logic;
  signal hmeo : std_logic_vector(4 downto 3);
begin
  j : entity work.m
    port map (ffgjntbh => hmeo, rsijux => dycmttlj, vgrgkxu => dicy);
  ntta : entity work.m
    port map (ffgjntbh => hmeo, rsijux => z, vgrgkxu => ebvdz);
  e : entity work.m
    port map (ffgjntbh => hmeo, rsijux => woropnnl, vgrgkxu => obg);
  iiswj : entity work.m
    port map (ffgjntbh => hmeo, rsijux => woropnnl, vgrgkxu => cfghbauue);
  
  -- Multi-driven assignments
  dycmttlj <= dycmttlj;
  hmeo <= hmeo;
  dycmttlj <= dycmttlj;
  hmeo <= hmeo;
end vhsegwnj;

library ieee;
use ieee.std_logic_1164.all;

entity uvtl is
  port (uvib : out real; bx : buffer std_logic; ienpxsw : in std_logic; oxb : buffer std_logic_vector(2 downto 4));
end uvtl;

library ieee;
use ieee.std_logic_1164.all;

architecture xme of uvtl is
  signal lr : boolean_vector(4 downto 3);
  signal odmznsmfv : std_logic;
  signal tqfjlrba : std_logic_vector(4 downto 3);
  signal vmkktcklh : boolean_vector(4 downto 3);
  signal giee : std_logic;
  signal vhh : std_logic_vector(4 downto 3);
begin
  havjm : entity work.m
    port map (ffgjntbh => vhh, rsijux => giee, vgrgkxu => vmkktcklh);
  vzqgqoa : entity work.m
    port map (ffgjntbh => tqfjlrba, rsijux => odmznsmfv, vgrgkxu => lr);
  
  -- Single-driven assignments
  uvib <= uvib;
  
  -- Multi-driven assignments
  vhh <= tqfjlrba;
  oxb <= oxb;
  oxb <= oxb;
end xme;

entity smahif is
  port (eck : buffer time; wono : in real; vkwtf : buffer real);
end smahif;

library ieee;
use ieee.std_logic_1164.all;

architecture jwu of smahif is
  signal oibqfbd : boolean_vector(4 downto 3);
  signal z : std_logic_vector(4 downto 3);
  signal cnfvogmswi : boolean_vector(4 downto 3);
  signal lrkrvsj : boolean_vector(4 downto 3);
  signal jtfh : std_logic;
  signal alf : std_logic_vector(4 downto 3);
begin
  ccuujzecn : entity work.m
    port map (ffgjntbh => alf, rsijux => jtfh, vgrgkxu => lrkrvsj);
  gqq : entity work.cqoijsbu
    port map (kgqzxbfpjw => jtfh);
  qj : entity work.m
    port map (ffgjntbh => alf, rsijux => jtfh, vgrgkxu => cnfvogmswi);
  hlsekkvue : entity work.m
    port map (ffgjntbh => z, rsijux => jtfh, vgrgkxu => oibqfbd);
  
  -- Single-driven assignments
  vkwtf <= vkwtf;
  eck <= eck;
  
  -- Multi-driven assignments
  alf <= alf;
  alf <= ('U', '-');
  alf <= alf;
end jwu;



-- Seed after: 5892922773386255070,4292249356257567981
