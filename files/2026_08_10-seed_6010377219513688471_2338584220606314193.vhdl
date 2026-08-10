-- Seed: 6010377219513688471,2338584220606314193

library ieee;
use ieee.std_logic_1164.all;

entity y is
  port (fcepyfdo : in real; dhnahks : out std_logic_vector(4 downto 4); kzsxxg : buffer std_logic);
end y;

architecture aior of y is
  
begin
  
end aior;

library ieee;
use ieee.std_logic_1164.all;

entity dptuxu is
  port (hfcll : out std_logic_vector(3 downto 4));
end dptuxu;

library ieee;
use ieee.std_logic_1164.all;

architecture efeuk of dptuxu is
  signal zwthtw : std_logic;
  signal klyffx : std_logic_vector(4 downto 4);
  signal yjosk : real;
begin
  bdfxti : entity work.y
    port map (fcepyfdo => yjosk, dhnahks => klyffx, kzsxxg => zwthtw);
  
  -- Single-driven assignments
  yjosk <= yjosk;
  
  -- Multi-driven assignments
  klyffx <= "1";
  hfcll <= "";
end efeuk;

entity nwxh is
  port (saee : linkage character; pkdok : buffer integer);
end nwxh;

library ieee;
use ieee.std_logic_1164.all;

architecture orzo of nwxh is
  signal yyt : real;
  signal vgi : std_logic_vector(4 downto 4);
  signal jty : std_logic;
  signal qj : std_logic;
  signal p : std_logic_vector(4 downto 4);
  signal iptu : real;
begin
  am : entity work.y
    port map (fcepyfdo => iptu, dhnahks => p, kzsxxg => qj);
  edl : entity work.y
    port map (fcepyfdo => iptu, dhnahks => p, kzsxxg => jty);
  uwixqwxli : entity work.y
    port map (fcepyfdo => iptu, dhnahks => vgi, kzsxxg => jty);
  xtgfo : entity work.y
    port map (fcepyfdo => yyt, dhnahks => vgi, kzsxxg => qj);
  
  -- Multi-driven assignments
  vgi <= (others => '0');
  p <= p;
  p <= p;
end orzo;



-- Seed after: 927208914652406673,2338584220606314193
