-- Seed: 9189928716752240041,13592003931158285879

library ieee;
use ieee.std_logic_1164.all;

entity qkx is
  port (mnosye : buffer std_logic; c : buffer time_vector(0 downto 0); s : out std_logic_vector(2 downto 2); vi : buffer real);
end qkx;

architecture z of qkx is
  
begin
  -- Single-driven assignments
  vi <= 16#EA635.C_9_C#;
  c <= (others => 34020.4_1_4_4 ns);
end z;

entity fzyerdc is
  port (ce : out integer; njmjm : out integer_vector(0 to 2); wtf : inout integer; pkvnggo : linkage severity_level);
end fzyerdc;

library ieee;
use ieee.std_logic_1164.all;

architecture h of fzyerdc is
  signal kqwgdbzl : real;
  signal aaphiuynk : std_logic_vector(2 downto 2);
  signal tohxdzb : time_vector(0 downto 0);
  signal qlrods : std_logic;
  signal vtmyppngbk : real;
  signal tm : std_logic_vector(2 downto 2);
  signal g : time_vector(0 downto 0);
  signal blqn : real;
  signal gcwpfxppco : time_vector(0 downto 0);
  signal b : std_logic;
  signal wfqob : real;
  signal yjnt : std_logic_vector(2 downto 2);
  signal dwoxo : time_vector(0 downto 0);
  signal jlasgbr : std_logic;
begin
  ruk : entity work.qkx
    port map (mnosye => jlasgbr, c => dwoxo, s => yjnt, vi => wfqob);
  pgjolz : entity work.qkx
    port map (mnosye => b, c => gcwpfxppco, s => yjnt, vi => blqn);
  fdobsy : entity work.qkx
    port map (mnosye => jlasgbr, c => g, s => tm, vi => vtmyppngbk);
  qamagizt : entity work.qkx
    port map (mnosye => qlrods, c => tohxdzb, s => aaphiuynk, vi => kqwgdbzl);
  
  -- Multi-driven assignments
  jlasgbr <= b;
  tm <= "H";
  b <= 'L';
  tm <= yjnt;
end h;



-- Seed after: 17651806524148459797,13592003931158285879
