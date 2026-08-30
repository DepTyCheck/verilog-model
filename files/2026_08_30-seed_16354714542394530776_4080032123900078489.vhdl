-- Seed: 16354714542394530776,4080032123900078489

library ieee;
use ieee.std_logic_1164.all;

entity njblr is
  port (eodjh : linkage std_logic; vbhx : in real_vector(1 downto 3); rqldtwwfq : buffer std_logic; v : inout bit);
end njblr;

architecture elaif of njblr is
  
begin
  -- Single-driven assignments
  v <= '0';
  
  -- Multi-driven assignments
  rqldtwwfq <= rqldtwwfq;
  rqldtwwfq <= rqldtwwfq;
  rqldtwwfq <= rqldtwwfq;
end elaif;

library ieee;
use ieee.std_logic_1164.all;

entity ovv is
  port (vajxchjokv : in std_logic_vector(3 to 3));
end ovv;

library ieee;
use ieee.std_logic_1164.all;

architecture jpunofd of ovv is
  signal uzmk : bit;
  signal tqou : std_logic;
  signal beyqtie : bit;
  signal vgkvfy : std_logic;
  signal tiirhp : bit;
  signal lh : bit;
  signal qkl : real_vector(1 downto 3);
  signal pirexofx : std_logic;
begin
  elkzsqxup : entity work.njblr
    port map (eodjh => pirexofx, vbhx => qkl, rqldtwwfq => pirexofx, v => lh);
  jq : entity work.njblr
    port map (eodjh => pirexofx, vbhx => qkl, rqldtwwfq => pirexofx, v => tiirhp);
  pj : entity work.njblr
    port map (eodjh => vgkvfy, vbhx => qkl, rqldtwwfq => pirexofx, v => beyqtie);
  r : entity work.njblr
    port map (eodjh => tqou, vbhx => qkl, rqldtwwfq => vgkvfy, v => uzmk);
  
  -- Single-driven assignments
  qkl <= (others => 0.0);
  
  -- Multi-driven assignments
  pirexofx <= '1';
  tqou <= 'L';
  pirexofx <= 'X';
end jpunofd;



-- Seed after: 13780627313655240690,4080032123900078489
