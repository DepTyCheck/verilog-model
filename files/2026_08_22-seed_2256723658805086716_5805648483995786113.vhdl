-- Seed: 2256723658805086716,5805648483995786113

library ieee;
use ieee.std_logic_1164.all;

entity j is
  port (dicocr : linkage std_logic_vector(2 downto 1));
end j;

architecture fd of j is
  
begin
  
end fd;

entity qdo is
  port (mqudiawx : inout boolean; owl : buffer character; uwhrsas : inout severity_level);
end qdo;

library ieee;
use ieee.std_logic_1164.all;

architecture rnvxfnwjiq of qdo is
  signal lkkzupepy : std_logic_vector(2 downto 1);
  signal hqwwhc : std_logic_vector(2 downto 1);
  signal dlfo : std_logic_vector(2 downto 1);
  signal ytvgydlftp : std_logic_vector(2 downto 1);
begin
  lhkrdso : entity work.j
    port map (dicocr => ytvgydlftp);
  utlyoljx : entity work.j
    port map (dicocr => dlfo);
  gphrs : entity work.j
    port map (dicocr => hqwwhc);
  ojxynndmh : entity work.j
    port map (dicocr => lkkzupepy);
  
  -- Multi-driven assignments
  hqwwhc <= ytvgydlftp;
  ytvgydlftp <= "UZ";
  ytvgydlftp <= hqwwhc;
  ytvgydlftp <= hqwwhc;
end rnvxfnwjiq;

entity wkdnklq is
  port (mrhmt : inout character; l : linkage time);
end wkdnklq;

library ieee;
use ieee.std_logic_1164.all;

architecture ihl of wkdnklq is
  signal o : std_logic_vector(2 downto 1);
  signal pg : std_logic_vector(2 downto 1);
  signal ohqsk : std_logic_vector(2 downto 1);
  signal pdvvunkpn : severity_level;
  signal w : boolean;
begin
  xcjqgezxtd : entity work.qdo
    port map (mqudiawx => w, owl => mrhmt, uwhrsas => pdvvunkpn);
  v : entity work.j
    port map (dicocr => ohqsk);
  u : entity work.j
    port map (dicocr => pg);
  xkeozfxor : entity work.j
    port map (dicocr => o);
end ihl;

library ieee;
use ieee.std_logic_1164.all;

entity zahggjaxs is
  port (uvx : out boolean_vector(2 to 3); uslxrpng : buffer std_logic_vector(2 to 1));
end zahggjaxs;

architecture tgr of zahggjaxs is
  signal jnenrq : severity_level;
  signal czvycdkkvd : character;
  signal vio : boolean;
begin
  rpkk : entity work.qdo
    port map (mqudiawx => vio, owl => czvycdkkvd, uwhrsas => jnenrq);
  
  -- Single-driven assignments
  uvx <= (TRUE, TRUE);
  
  -- Multi-driven assignments
  uslxrpng <= (others => '0');
  uslxrpng <= "";
  uslxrpng <= (others => '0');
end tgr;



-- Seed after: 2259831710639093049,5805648483995786113
