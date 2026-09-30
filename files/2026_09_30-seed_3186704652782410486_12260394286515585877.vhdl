-- Seed: 3186704652782410486,12260394286515585877

library ieee;
use ieee.std_logic_1164.all;

entity pvtys is
  port ( uuo : out time_vector(1 downto 4)
  ; tqwkkidu : buffer std_logic_vector(3 to 3)
  ; ac : inout std_logic_vector(4 downto 0)
  ; jfytidn : inout std_logic_vector(2 downto 4)
  );
end pvtys;

architecture a of pvtys is
  
begin
  -- Single-driven assignments
  uuo <= uuo;
  
  -- Multi-driven assignments
  jfytidn <= jfytidn;
  jfytidn <= "";
  jfytidn <= (others => '0');
  jfytidn <= jfytidn;
end a;

entity zgqrrwxp is
  port (yzwjwec : inout time);
end zgqrrwxp;

library ieee;
use ieee.std_logic_1164.all;

architecture b of zgqrrwxp is
  signal bghplgrkex : std_logic_vector(2 downto 4);
  signal cv : std_logic_vector(4 downto 0);
  signal ibryowrwvc : std_logic_vector(3 to 3);
  signal tutemk : time_vector(1 downto 4);
  signal ycc : time_vector(1 downto 4);
  signal iwmehna : std_logic_vector(2 downto 4);
  signal j : std_logic_vector(3 to 3);
  signal mfgyvkor : time_vector(1 downto 4);
  signal hcum : std_logic_vector(2 downto 4);
  signal jh : std_logic_vector(4 downto 0);
  signal kvu : std_logic_vector(3 to 3);
  signal itgzzab : time_vector(1 downto 4);
begin
  zooqh : entity work.pvtys
    port map (uuo => itgzzab, tqwkkidu => kvu, ac => jh, jfytidn => hcum);
  bndsu : entity work.pvtys
    port map (uuo => mfgyvkor, tqwkkidu => j, ac => jh, jfytidn => iwmehna);
  jiwzfzii : entity work.pvtys
    port map (uuo => ycc, tqwkkidu => kvu, ac => jh, jfytidn => iwmehna);
  aof : entity work.pvtys
    port map (uuo => tutemk, tqwkkidu => ibryowrwvc, ac => cv, jfytidn => bghplgrkex);
  
  -- Single-driven assignments
  yzwjwec <= 2_2_0 ms;
  
  -- Multi-driven assignments
  kvu <= (others => 'U');
  kvu <= "1";
  iwmehna <= (others => '0');
end b;



-- Seed after: 14426842090845557014,12260394286515585877
