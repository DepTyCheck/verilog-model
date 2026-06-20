-- Seed: 13197023536109553218,3924983747739634027

entity ofiyvwem is
  port (luaaefhow : inout time; num : linkage bit_vector(2 downto 3); odyecj : linkage real);
end ofiyvwem;

architecture m of ofiyvwem is
  
begin
  -- Single-driven assignments
  luaaefhow <= 2 min;
end m;

library ieee;
use ieee.std_logic_1164.all;

entity fcfh is
  port (mkvuv : out std_logic);
end fcfh;

architecture sy of fcfh is
  signal n : real;
  signal iesdbwiai : bit_vector(2 downto 3);
  signal ybsnspcoy : time;
begin
  wjnpabujmp : entity work.ofiyvwem
    port map (luaaefhow => ybsnspcoy, num => iesdbwiai, odyecj => n);
  
  -- Multi-driven assignments
  mkvuv <= 'L';
end sy;

entity vkto is
  port (tdoyko : inout integer);
end vkto;

library ieee;
use ieee.std_logic_1164.all;

architecture wbc of vkto is
  signal gfuw : real;
  signal iqnxhx : bit_vector(2 downto 3);
  signal yvvniicqwx : time;
  signal ewrb : real;
  signal hwbcqu : bit_vector(2 downto 3);
  signal rqoymwbup : time;
  signal dyxr : real;
  signal elxblw : bit_vector(2 downto 3);
  signal avxqt : time;
  signal iuza : std_logic;
begin
  dypbxklar : entity work.fcfh
    port map (mkvuv => iuza);
  hqtxwchnas : entity work.ofiyvwem
    port map (luaaefhow => avxqt, num => elxblw, odyecj => dyxr);
  nhe : entity work.ofiyvwem
    port map (luaaefhow => rqoymwbup, num => hwbcqu, odyecj => ewrb);
  rsvnxbg : entity work.ofiyvwem
    port map (luaaefhow => yvvniicqwx, num => iqnxhx, odyecj => gfuw);
  
  -- Single-driven assignments
  tdoyko <= 243;
end wbc;



-- Seed after: 9367727742901883312,3924983747739634027
