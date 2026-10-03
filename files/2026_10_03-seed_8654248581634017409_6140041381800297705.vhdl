-- Seed: 8654248581634017409,6140041381800297705

library ieee;
use ieee.std_logic_1164.all;

entity thze is
  port (hgkavnsw : buffer std_logic; mttmuzp : in std_logic; l : buffer integer; k : in std_logic);
end thze;

architecture lmdl of thze is
  
begin
  -- Single-driven assignments
  l <= 4;
end lmdl;

entity c is
  port (tp : inout time);
end c;

library ieee;
use ieee.std_logic_1164.all;

architecture mcizlg of c is
  signal tmebdkeva : integer;
  signal uxbxny : std_logic;
  signal kfqtw : std_logic;
  signal jr : integer;
  signal rmyr : integer;
  signal ypdpkif : std_logic;
  signal alahbqef : std_logic;
  signal nxm : integer;
  signal mqq : std_logic;
  signal jkoww : std_logic;
begin
  lqfdfjcsw : entity work.thze
    port map (hgkavnsw => jkoww, mttmuzp => mqq, l => nxm, k => alahbqef);
  ommey : entity work.thze
    port map (hgkavnsw => ypdpkif, mttmuzp => alahbqef, l => rmyr, k => alahbqef);
  ahxxyfuzm : entity work.thze
    port map (hgkavnsw => ypdpkif, mttmuzp => jkoww, l => jr, k => jkoww);
  ltuydghi : entity work.thze
    port map (hgkavnsw => kfqtw, mttmuzp => uxbxny, l => tmebdkeva, k => uxbxny);
  
  -- Multi-driven assignments
  uxbxny <= jkoww;
  uxbxny <= jkoww;
end mcizlg;



-- Seed after: 3195430789977445550,6140041381800297705
