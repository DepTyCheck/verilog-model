-- Seed: 8274226909274135684,499459191852795575

library ieee;
use ieee.std_logic_1164.all;

entity sxwdirpo is
  port (dzbsfk : out integer; gfoceazhcp : inout time; qovoz : linkage std_logic_vector(0 downto 4));
end sxwdirpo;

architecture kebk of sxwdirpo is
  
begin
  
end kebk;

entity yjgqh is
  port (yytydektye : buffer boolean; ilxk : inout real);
end yjgqh;

library ieee;
use ieee.std_logic_1164.all;

architecture twvhshrnv of yjgqh is
  signal pctpjlh : time;
  signal u : integer;
  signal bq : std_logic_vector(0 downto 4);
  signal wafwbuvszn : time;
  signal kwanqgqqi : integer;
begin
  mzls : entity work.sxwdirpo
    port map (dzbsfk => kwanqgqqi, gfoceazhcp => wafwbuvszn, qovoz => bq);
  dryham : entity work.sxwdirpo
    port map (dzbsfk => u, gfoceazhcp => pctpjlh, qovoz => bq);
  
  -- Single-driven assignments
  ilxk <= 2#0.00110#;
  yytydektye <= yytydektye;
end twvhshrnv;

entity kqradi is
  port (yfj : inout integer_vector(4 downto 0); f : linkage bit);
end kqradi;

library ieee;
use ieee.std_logic_1164.all;

architecture qtjw of kqradi is
  signal uug : real;
  signal qtcf : boolean;
  signal jlgszjxfc : std_logic_vector(0 downto 4);
  signal k : time;
  signal xd : integer;
  signal sqckniv : real;
  signal kgimqtsl : boolean;
  signal qtuhl : std_logic_vector(0 downto 4);
  signal cir : time;
  signal yxozyr : integer;
begin
  hrg : entity work.sxwdirpo
    port map (dzbsfk => yxozyr, gfoceazhcp => cir, qovoz => qtuhl);
  cd : entity work.yjgqh
    port map (yytydektye => kgimqtsl, ilxk => sqckniv);
  gc : entity work.sxwdirpo
    port map (dzbsfk => xd, gfoceazhcp => k, qovoz => jlgszjxfc);
  mibwebmhrg : entity work.yjgqh
    port map (yytydektye => qtcf, ilxk => uug);
  
  -- Multi-driven assignments
  jlgszjxfc <= (others => '0');
  qtuhl <= qtuhl;
  qtuhl <= (others => '0');
end qtjw;



-- Seed after: 14590971629612220894,499459191852795575
