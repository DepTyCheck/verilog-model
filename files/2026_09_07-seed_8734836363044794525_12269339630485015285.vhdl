-- Seed: 8734836363044794525,12269339630485015285

library ieee;
use ieee.std_logic_1164.all;

entity pxtxkfys is
  port (camxraphjx : buffer std_logic);
end pxtxkfys;

architecture hcp of pxtxkfys is
  
begin
  -- Multi-driven assignments
  camxraphjx <= 'W';
  camxraphjx <= camxraphjx;
  camxraphjx <= 'Z';
  camxraphjx <= 'W';
end hcp;

entity hzrjggvdhc is
  port (ejquc : in time; oxyucqqc : in time);
end hzrjggvdhc;

library ieee;
use ieee.std_logic_1164.all;

architecture sff of hzrjggvdhc is
  signal nukafzmhrj : std_logic;
begin
  i : entity work.pxtxkfys
    port map (camxraphjx => nukafzmhrj);
  
  -- Multi-driven assignments
  nukafzmhrj <= '1';
end sff;

entity ebtffs is
  port (xngkbunsjy : inout time);
end ebtffs;

library ieee;
use ieee.std_logic_1164.all;

architecture ogucaaqfd of ebtffs is
  signal qlabytn : std_logic;
begin
  ekqjttx : entity work.pxtxkfys
    port map (camxraphjx => qlabytn);
  
  -- Single-driven assignments
  xngkbunsjy <= 2 min;
  
  -- Multi-driven assignments
  qlabytn <= qlabytn;
  qlabytn <= '-';
end ogucaaqfd;



-- Seed after: 13242257268174721726,12269339630485015285
