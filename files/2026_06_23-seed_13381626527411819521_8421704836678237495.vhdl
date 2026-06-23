-- Seed: 13381626527411819521,8421704836678237495

library ieee;
use ieee.std_logic_1164.all;

entity nnyehd is
  port (uo : out std_logic_vector(4 downto 2));
end nnyehd;

architecture pxfipfkqtd of nnyehd is
  
begin
  -- Multi-driven assignments
  uo <= ('1', 'U', 'Z');
  uo <= ('0', '1', 'W');
  uo <= "ZH-";
end pxfipfkqtd;

library ieee;
use ieee.std_logic_1164.all;

entity poxtpgksml is
  port (koxbdg : out std_logic; vgmai : out std_logic; jaty : buffer std_logic_vector(0 to 3));
end poxtpgksml;

library ieee;
use ieee.std_logic_1164.all;

architecture rrc of poxtpgksml is
  signal gjjuyitc : std_logic_vector(4 downto 2);
begin
  dpdnttoy : entity work.nnyehd
    port map (uo => gjjuyitc);
  rxyk : entity work.nnyehd
    port map (uo => gjjuyitc);
  
  -- Multi-driven assignments
  gjjuyitc <= ('-', '-', 'U');
  jaty <= ('1', '-', '-', '0');
  jaty <= "W1-W";
end rrc;

entity vz is
  port (eszwncl : buffer real);
end vz;

architecture h of vz is
  
begin
  -- Single-driven assignments
  eszwncl <= 0_4_1_1.23234;
end h;

entity nwpvtdc is
  port (guszz : linkage character; npfexgwgcl : inout severity_level);
end nwpvtdc;

library ieee;
use ieee.std_logic_1164.all;

architecture tcqju of nwpvtdc is
  signal uucrjuqxy : std_logic_vector(0 to 3);
  signal sme : std_logic;
  signal a : std_logic;
  signal xlcg : std_logic_vector(4 downto 2);
  signal nj : real;
begin
  yqnifqnxue : entity work.vz
    port map (eszwncl => nj);
  wknj : entity work.nnyehd
    port map (uo => xlcg);
  ot : entity work.poxtpgksml
    port map (koxbdg => a, vgmai => sme, jaty => uucrjuqxy);
  inwsmn : entity work.nnyehd
    port map (uo => xlcg);
  
  -- Single-driven assignments
  npfexgwgcl <= ERROR;
  
  -- Multi-driven assignments
  sme <= 'H';
end tcqju;



-- Seed after: 17622080397604407085,8421704836678237495
