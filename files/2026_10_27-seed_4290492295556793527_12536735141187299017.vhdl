-- Seed: 4290492295556793527,12536735141187299017

library ieee;
use ieee.std_logic_1164.all;

entity fxrokh is
  port (lbyl : linkage std_logic_vector(2 to 1); ew : buffer std_logic_vector(1 to 4));
end fxrokh;

architecture wkenofmsz of fxrokh is
  
begin
  -- Multi-driven assignments
  ew <= "UZZX";
  ew <= ew;
  ew <= "10U0";
  ew <= ew;
end wkenofmsz;

library ieee;
use ieee.std_logic_1164.all;

entity tzuqhtsi is
  port (c : inout integer_vector(4 to 1); hoxxc : buffer std_logic_vector(3 downto 1); sbtt : in std_logic_vector(4 to 4));
end tzuqhtsi;

library ieee;
use ieee.std_logic_1164.all;

architecture oyn of tzuqhtsi is
  signal vhlsh : std_logic_vector(1 to 4);
  signal sxoin : std_logic_vector(1 to 4);
  signal xu : std_logic_vector(1 to 4);
  signal lexqfav : std_logic_vector(2 to 1);
begin
  hogjngtcz : entity work.fxrokh
    port map (lbyl => lexqfav, ew => xu);
  grxiqiqzpk : entity work.fxrokh
    port map (lbyl => lexqfav, ew => sxoin);
  ovhibnlac : entity work.fxrokh
    port map (lbyl => lexqfav, ew => xu);
  lsjo : entity work.fxrokh
    port map (lbyl => lexqfav, ew => vhlsh);
  
  -- Single-driven assignments
  c <= (others => 0);
  
  -- Multi-driven assignments
  lexqfav <= lexqfav;
  vhlsh <= xu;
  xu <= xu;
end oyn;



-- Seed after: 5238142240131182384,12536735141187299017
