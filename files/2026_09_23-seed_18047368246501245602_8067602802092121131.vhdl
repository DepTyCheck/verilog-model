-- Seed: 18047368246501245602,8067602802092121131

library ieee;
use ieee.std_logic_1164.all;

entity nqysp is
  port (zgtrlr : out boolean_vector(3 to 2); eocxwce : buffer std_logic; qiy : out bit; hn : out integer);
end nqysp;

architecture fthru of nqysp is
  
begin
  -- Single-driven assignments
  zgtrlr <= zgtrlr;
  qiy <= qiy;
  
  -- Multi-driven assignments
  eocxwce <= eocxwce;
end fthru;

entity keqd is
  port (hjnysxwul : out time; wycqyljxkq : linkage bit_vector(2 downto 3); redfju : inout integer; x : buffer time_vector(0 to 1));
end keqd;

library ieee;
use ieee.std_logic_1164.all;

architecture m of keqd is
  signal dpogpe : integer;
  signal jlly : bit;
  signal d : std_logic;
  signal azyj : boolean_vector(3 to 2);
  signal slpee : integer;
  signal lwctpm : bit;
  signal wbi : std_logic;
  signal dp : boolean_vector(3 to 2);
begin
  bdcx : entity work.nqysp
    port map (zgtrlr => dp, eocxwce => wbi, qiy => lwctpm, hn => slpee);
  ju : entity work.nqysp
    port map (zgtrlr => azyj, eocxwce => d, qiy => jlly, hn => dpogpe);
  
  -- Single-driven assignments
  redfju <= redfju;
  
  -- Multi-driven assignments
  wbi <= 'U';
end m;



-- Seed after: 14376969637604911932,8067602802092121131
