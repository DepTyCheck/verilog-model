-- Seed: 15816929345604786576,12143220691580258643

library ieee;
use ieee.std_logic_1164.all;

entity wwolfzp is
  port (szmqq : in std_logic; uauyhvxbj : out time_vector(0 to 1); drrwvscte : in time);
end wwolfzp;

architecture sgakdbnbxb of wwolfzp is
  
begin
  -- Single-driven assignments
  uauyhvxbj <= (1 hr, 3.3_4 ps);
end sgakdbnbxb;

library ieee;
use ieee.std_logic_1164.all;

entity bnydialab is
  port (tcazpup : in std_logic; usb : in real);
end bnydialab;

architecture t of bnydialab is
  
begin
  
end t;

library ieee;
use ieee.std_logic_1164.all;

entity aqmdivscpe is
  port (kipuk : buffer time; oivfbjltfj : in integer; uqr : buffer std_logic_vector(2 downto 2));
end aqmdivscpe;

library ieee;
use ieee.std_logic_1164.all;

architecture xggt of aqmdivscpe is
  signal veo : time_vector(0 to 1);
  signal vzxyhd : time;
  signal litqccoyq : time_vector(0 to 1);
  signal y : std_logic;
  signal diopvwqqeo : time_vector(0 to 1);
  signal g : std_logic;
begin
  ykzfnp : entity work.wwolfzp
    port map (szmqq => g, uauyhvxbj => diopvwqqeo, drrwvscte => kipuk);
  xq : entity work.wwolfzp
    port map (szmqq => y, uauyhvxbj => litqccoyq, drrwvscte => vzxyhd);
  csqhiz : entity work.wwolfzp
    port map (szmqq => g, uauyhvxbj => veo, drrwvscte => kipuk);
  
  -- Single-driven assignments
  kipuk <= vzxyhd;
  vzxyhd <= 0 hr;
  
  -- Multi-driven assignments
  uqr <= uqr;
  uqr <= uqr;
end xggt;

entity tihvp is
  port (gywoaiqe : linkage real);
end tihvp;

library ieee;
use ieee.std_logic_1164.all;

architecture bpmpmetemy of tihvp is
  signal cxhd : time;
  signal u : time_vector(0 to 1);
  signal arvzuqbw : std_logic;
  signal dksoxcxcuu : std_logic_vector(2 downto 2);
  signal kvrtp : time;
  signal yl : std_logic_vector(2 downto 2);
  signal potseiwew : integer;
  signal i : time;
begin
  sffe : entity work.aqmdivscpe
    port map (kipuk => i, oivfbjltfj => potseiwew, uqr => yl);
  ewd : entity work.aqmdivscpe
    port map (kipuk => kvrtp, oivfbjltfj => potseiwew, uqr => dksoxcxcuu);
  ckpwgnzei : entity work.wwolfzp
    port map (szmqq => arvzuqbw, uauyhvxbj => u, drrwvscte => cxhd);
  
  -- Multi-driven assignments
  yl <= (others => 'W');
end bpmpmetemy;



-- Seed after: 14637534384680197957,12143220691580258643
