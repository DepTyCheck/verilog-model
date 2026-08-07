-- Seed: 12750960927997648354,8068158652091157513

library ieee;
use ieee.std_logic_1164.all;

entity mwvcg is
  port (rhgqo : inout time; hnduilwl : out std_logic; jksheue : in time);
end mwvcg;

architecture wzs of mwvcg is
  
begin
  -- Single-driven assignments
  rhgqo <= 0_0_0 ps;
  
  -- Multi-driven assignments
  hnduilwl <= hnduilwl;
  hnduilwl <= hnduilwl;
  hnduilwl <= hnduilwl;
  hnduilwl <= hnduilwl;
end wzs;

library ieee;
use ieee.std_logic_1164.all;

entity ikoqxoljl is
  port (cmalngzyu : linkage real; uppbha : inout std_logic; oipxkwagr : in time);
end ikoqxoljl;

architecture kazwtpy of ikoqxoljl is
  signal beawvqznrl : time;
  signal vnah : time;
  signal o : time;
begin
  gslgrml : entity work.mwvcg
    port map (rhgqo => o, hnduilwl => uppbha, jksheue => oipxkwagr);
  dngq : entity work.mwvcg
    port map (rhgqo => vnah, hnduilwl => uppbha, jksheue => o);
  mvuywjdcxc : entity work.mwvcg
    port map (rhgqo => beawvqznrl, hnduilwl => uppbha, jksheue => oipxkwagr);
  
  -- Multi-driven assignments
  uppbha <= 'U';
  uppbha <= 'X';
  uppbha <= 'L';
end kazwtpy;



-- Seed after: 4757411869933655277,8068158652091157513
