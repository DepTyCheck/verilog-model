-- Seed: 8621171018050694988,7311216359267151659

library ieee;
use ieee.std_logic_1164.all;

entity du is
  port (lkoydm : out std_logic; c : out integer);
end du;

architecture eeydhgpxpq of du is
  
begin
  -- Single-driven assignments
  c <= 16#F11#;
end eeydhgpxpq;

entity jutj is
  port (ste : out boolean; inhgvbn : in time);
end jutj;

architecture byjkpmfx of jutj is
  
begin
  -- Single-driven assignments
  ste <= FALSE;
end byjkpmfx;

entity fdpffcw is
  port (bma : linkage time_vector(4 downto 0); axtlxaniz : buffer bit_vector(0 downto 1));
end fdpffcw;

library ieee;
use ieee.std_logic_1164.all;

architecture zvwcx of fdpffcw is
  signal qmlkgal : integer;
  signal q : integer;
  signal osfndwym : std_logic;
  signal uznozpxcn : integer;
  signal nozclvhy : std_logic;
begin
  iha : entity work.du
    port map (lkoydm => nozclvhy, c => uznozpxcn);
  hdjkgcr : entity work.du
    port map (lkoydm => osfndwym, c => q);
  wycuob : entity work.du
    port map (lkoydm => nozclvhy, c => qmlkgal);
  
  -- Multi-driven assignments
  nozclvhy <= '1';
  nozclvhy <= 'L';
end zvwcx;



-- Seed after: 17695451883791729780,7311216359267151659
