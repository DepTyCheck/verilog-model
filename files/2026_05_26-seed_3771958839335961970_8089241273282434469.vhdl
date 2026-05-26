-- Seed: 3771958839335961970,8089241273282434469

library ieee;
use ieee.std_logic_1164.all;

entity l is
  port (cb : linkage bit_vector(2 downto 2); uzwy : linkage std_logic);
end l;



architecture kaj of l is
  
begin
  
end kaj;

library ieee;
use ieee.std_logic_1164.all;

entity dgztcfodl is
  port (hfikretw : linkage boolean; k : out std_logic; hudx : inout real; bxd : buffer integer);
end dgztcfodl;



architecture zqonofs of dgztcfodl is
  signal ubxrtiqyp : bit_vector(2 downto 2);
  signal qfyasea : bit_vector(2 downto 2);
begin
  oe : entity work.l
    port map (cb => qfyasea, uzwy => k);
  viand : entity work.l
    port map (cb => ubxrtiqyp, uzwy => k);
end zqonofs;

library ieee;
use ieee.std_logic_1164.all;

entity tmpceapit is
  port (d : inout integer; r : buffer std_logic; wopycybzf : buffer std_logic);
end tmpceapit;



architecture u of tmpceapit is
  signal hzwvoudttp : integer;
  signal pvnaewr : real;
  signal fn : boolean;
  signal lque : integer;
  signal bxebunan : real;
  signal ikifa : boolean;
begin
  dqf : entity work.dgztcfodl
    port map (hfikretw => ikifa, k => wopycybzf, hudx => bxebunan, bxd => lque);
  i : entity work.dgztcfodl
    port map (hfikretw => fn, k => wopycybzf, hudx => pvnaewr, bxd => hzwvoudttp);
end u;

library ieee;
use ieee.std_logic_1164.all;

entity opwv is
  port (nzdew : linkage bit; thfe : out std_logic);
end opwv;



architecture qqbsw of opwv is
  signal dnbt : integer;
  signal fyeeprarwa : integer;
  signal jhpqex : real;
  signal ywuf : boolean;
begin
  itgunyj : entity work.dgztcfodl
    port map (hfikretw => ywuf, k => thfe, hudx => jhpqex, bxd => fyeeprarwa);
  pzk : entity work.tmpceapit
    port map (d => dnbt, r => thfe, wopycybzf => thfe);
end qqbsw;



-- Seed after: 498675017198722877,8089241273282434469
