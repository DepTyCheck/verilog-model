-- Seed: 12070204580524786836,511364357853360275

library ieee;
use ieee.std_logic_1164.all;

entity zrpldekwqo is
  port (aplqd : buffer std_logic_vector(3 to 4); alubkffum : buffer std_logic; psf : buffer std_logic_vector(2 to 1));
end zrpldekwqo;

architecture djhuxsghss of zrpldekwqo is
  
begin
  -- Multi-driven assignments
  aplqd <= "ZZ";
  psf <= psf;
end djhuxsghss;

entity a is
  port (ksyojhlqef : inout bit);
end a;

architecture nuis of a is
  
begin
  -- Single-driven assignments
  ksyojhlqef <= '0';
end nuis;

entity bbcdamu is
  port (zfphltkp : linkage real; sdhlekb : in integer);
end bbcdamu;

library ieee;
use ieee.std_logic_1164.all;

architecture buzblmys of bbcdamu is
  signal hma : bit;
  signal luhfeot : std_logic_vector(2 to 1);
  signal rixpeydkis : std_logic;
  signal ttcfb : std_logic_vector(2 to 1);
  signal sjrzqi : std_logic;
  signal lamfepal : std_logic_vector(3 to 4);
  signal nf : bit;
begin
  gqabivip : entity work.a
    port map (ksyojhlqef => nf);
  ucvpisyxr : entity work.zrpldekwqo
    port map (aplqd => lamfepal, alubkffum => sjrzqi, psf => ttcfb);
  jc : entity work.zrpldekwqo
    port map (aplqd => lamfepal, alubkffum => rixpeydkis, psf => luhfeot);
  shvoi : entity work.a
    port map (ksyojhlqef => hma);
  
  -- Multi-driven assignments
  lamfepal <= lamfepal;
  lamfepal <= ('U', 'H');
  rixpeydkis <= '0';
end buzblmys;



-- Seed after: 15209990713708601292,511364357853360275
