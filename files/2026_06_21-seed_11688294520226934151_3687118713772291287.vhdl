-- Seed: 11688294520226934151,3687118713772291287

library ieee;
use ieee.std_logic_1164.all;

entity oyhkw is
  port (fzliaqic : out std_logic; ycrnn : inout real; handp : linkage real_vector(4 downto 2); pggqkhacol : inout std_logic);
end oyhkw;

architecture lkebd of oyhkw is
  
begin
  -- Single-driven assignments
  ycrnn <= 8#1066.3_4_0_7_7#;
  
  -- Multi-driven assignments
  pggqkhacol <= '-';
end lkebd;

entity icdagjdngv is
  port (tocv : out severity_level);
end icdagjdngv;

library ieee;
use ieee.std_logic_1164.all;

architecture yyp of icdagjdngv is
  signal urfc : std_logic;
  signal eembyz : real_vector(4 downto 2);
  signal kshupi : real;
  signal qlo : std_logic;
begin
  wzgpeju : entity work.oyhkw
    port map (fzliaqic => qlo, ycrnn => kshupi, handp => eembyz, pggqkhacol => urfc);
  
  -- Multi-driven assignments
  qlo <= 'L';
  qlo <= 'U';
end yyp;

library ieee;
use ieee.std_logic_1164.all;

entity i is
  port (eajjw : linkage std_logic; cbhxmbli : linkage time);
end i;

library ieee;
use ieee.std_logic_1164.all;

architecture acwjxxqfr of i is
  signal aazuklq : std_logic;
  signal obrkfhplr : real_vector(4 downto 2);
  signal av : real;
  signal iiplkjthbv : std_logic;
  signal ztj : severity_level;
  signal cdifw : severity_level;
begin
  yxqx : entity work.icdagjdngv
    port map (tocv => cdifw);
  xktswixo : entity work.icdagjdngv
    port map (tocv => ztj);
  chyufgqro : entity work.oyhkw
    port map (fzliaqic => iiplkjthbv, ycrnn => av, handp => obrkfhplr, pggqkhacol => aazuklq);
end acwjxxqfr;

library ieee;
use ieee.std_logic_1164.all;

entity sc is
  port (wf : linkage severity_level; zlsstmh : buffer bit; jbr : inout std_logic_vector(4 downto 0));
end sc;

library ieee;
use ieee.std_logic_1164.all;

architecture woqfbhbh of sc is
  signal oiqslogzqu : real_vector(4 downto 2);
  signal aw : real;
  signal dicacvded : std_logic;
  signal pfbydkbuv : real_vector(4 downto 2);
  signal xluqwn : real;
  signal kgkel : std_logic;
  signal bey : std_logic;
  signal ywsjfegmv : real_vector(4 downto 2);
  signal hxirfysp : real;
  signal ssangzwk : std_logic;
  signal flnjtgf : real_vector(4 downto 2);
  signal ogppfgegq : real;
  signal j : std_logic;
begin
  jqna : entity work.oyhkw
    port map (fzliaqic => j, ycrnn => ogppfgegq, handp => flnjtgf, pggqkhacol => ssangzwk);
  tzrtda : entity work.oyhkw
    port map (fzliaqic => j, ycrnn => hxirfysp, handp => ywsjfegmv, pggqkhacol => bey);
  f : entity work.oyhkw
    port map (fzliaqic => kgkel, ycrnn => xluqwn, handp => pfbydkbuv, pggqkhacol => j);
  sh : entity work.oyhkw
    port map (fzliaqic => dicacvded, ycrnn => aw, handp => oiqslogzqu, pggqkhacol => j);
  
  -- Single-driven assignments
  zlsstmh <= '0';
  
  -- Multi-driven assignments
  jbr <= "ZXUL1";
  ssangzwk <= 'W';
  jbr <= ('X', 'U', 'L', 'W', 'U');
  jbr <= "XL00L";
end woqfbhbh;



-- Seed after: 16486875991874438532,3687118713772291287
