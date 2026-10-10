-- Seed: 4767629767303130434,511364357853360275

library ieee;
use ieee.std_logic_1164.all;

entity hjaq is
  port (wmmmwyjx : linkage std_logic_vector(0 downto 2));
end hjaq;

architecture hzbwor of hjaq is
  
begin
  
end hzbwor;

library ieee;
use ieee.std_logic_1164.all;

entity yrc is
  port (e : out std_logic);
end yrc;

library ieee;
use ieee.std_logic_1164.all;

architecture aotqsrldj of yrc is
  signal ikknwwo : std_logic_vector(0 downto 2);
begin
  uij : entity work.hjaq
    port map (wmmmwyjx => ikknwwo);
  
  -- Multi-driven assignments
  e <= e;
end aotqsrldj;

entity ib is
  port (py : out time; qi : inout bit; qccdxpbb : inout boolean; qopgarc : linkage time);
end ib;

library ieee;
use ieee.std_logic_1164.all;

architecture g of ib is
  signal mijkbcnj : std_logic;
  signal xngpbr : std_logic_vector(0 downto 2);
  signal hihghrc : std_logic_vector(0 downto 2);
begin
  eqnvobdsm : entity work.hjaq
    port map (wmmmwyjx => hihghrc);
  emtjrw : entity work.hjaq
    port map (wmmmwyjx => hihghrc);
  ewseclfmk : entity work.hjaq
    port map (wmmmwyjx => xngpbr);
  nez : entity work.yrc
    port map (e => mijkbcnj);
  
  -- Single-driven assignments
  qccdxpbb <= FALSE;
  py <= py;
  qi <= qi;
  
  -- Multi-driven assignments
  hihghrc <= (others => '0');
  xngpbr <= (others => '0');
  hihghrc <= (others => '0');
  hihghrc <= hihghrc;
end g;



-- Seed after: 12449857055176807682,511364357853360275
