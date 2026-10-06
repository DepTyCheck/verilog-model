-- Seed: 5779060024382145657,3042374792655995433

library ieee;
use ieee.std_logic_1164.all;

entity bvuzdrc is
  port (sdpewxuml : linkage std_logic_vector(2 downto 0));
end bvuzdrc;

architecture wmmxzqrzxi of bvuzdrc is
  
begin
  
end wmmxzqrzxi;

entity yqvr is
  port (yosbewslp : in time);
end yqvr;

library ieee;
use ieee.std_logic_1164.all;

architecture dz of yqvr is
  signal qyiqg : std_logic_vector(2 downto 0);
begin
  shovtgg : entity work.bvuzdrc
    port map (sdpewxuml => qyiqg);
  drzcub : entity work.bvuzdrc
    port map (sdpewxuml => qyiqg);
end dz;

library ieee;
use ieee.std_logic_1164.all;

entity mtq is
  port (wj : in std_logic_vector(1 downto 2));
end mtq;

library ieee;
use ieee.std_logic_1164.all;

architecture wt of mtq is
  signal guxnyj : std_logic_vector(2 downto 0);
  signal izuh : time;
  signal np : std_logic_vector(2 downto 0);
begin
  pgdo : entity work.bvuzdrc
    port map (sdpewxuml => np);
  fgesp : entity work.yqvr
    port map (yosbewslp => izuh);
  xwzuoiciox : entity work.bvuzdrc
    port map (sdpewxuml => np);
  afhgg : entity work.bvuzdrc
    port map (sdpewxuml => guxnyj);
  
  -- Single-driven assignments
  izuh <= 0 min;
  
  -- Multi-driven assignments
  np <= np;
  np <= np;
  np <= guxnyj;
  np <= ('H', 'L', 'X');
end wt;



-- Seed after: 16601492552207683352,3042374792655995433
