-- Seed: 6539438635038213774,5983430343285687595

library ieee;
use ieee.std_logic_1164.all;

entity he is
  port (jxei : inout std_logic; yxs : buffer real_vector(2 to 3); wslcrsfcto : buffer std_logic_vector(0 to 2));
end he;

architecture fvkjdareu of he is
  
begin
  -- Single-driven assignments
  yxs <= (16#D_6_1_9_E.3_7#, 16#3488.8#);
end fvkjdareu;

library ieee;
use ieee.std_logic_1164.all;

entity af is
  port (csffw : buffer time; wd : inout integer; hfskamqxpf : linkage std_logic; la : buffer std_logic_vector(4 downto 3));
end af;

library ieee;
use ieee.std_logic_1164.all;

architecture lfuoef of af is
  signal zjmloxzd : std_logic_vector(0 to 2);
  signal tujj : real_vector(2 to 3);
  signal ww : std_logic;
begin
  wfbvqvd : entity work.he
    port map (jxei => ww, yxs => tujj, wslcrsfcto => zjmloxzd);
  
  -- Single-driven assignments
  wd <= 16#F_E_5_E#;
  csffw <= csffw;
  
  -- Multi-driven assignments
  la <= "1L";
end lfuoef;

library ieee;
use ieee.std_logic_1164.all;

entity mztfjl is
  port (g : inout severity_level; nfzav : buffer std_logic; idtegqg : linkage std_logic);
end mztfjl;

library ieee;
use ieee.std_logic_1164.all;

architecture misugmi of mztfjl is
  signal xagfp : real_vector(2 to 3);
  signal gf : std_logic;
  signal udjeg : real_vector(2 to 3);
  signal xo : std_logic;
  signal aztrwep : std_logic_vector(0 to 2);
  signal iwusbeadk : real_vector(2 to 3);
  signal hsdn : std_logic_vector(4 downto 3);
  signal ga : integer;
  signal dkk : time;
begin
  wqyxfqb : entity work.af
    port map (csffw => dkk, wd => ga, hfskamqxpf => idtegqg, la => hsdn);
  manevcgi : entity work.he
    port map (jxei => nfzav, yxs => iwusbeadk, wslcrsfcto => aztrwep);
  wzrwokdye : entity work.he
    port map (jxei => xo, yxs => udjeg, wslcrsfcto => aztrwep);
  kzbnbpr : entity work.he
    port map (jxei => gf, yxs => xagfp, wslcrsfcto => aztrwep);
  
  -- Single-driven assignments
  g <= ERROR;
  
  -- Multi-driven assignments
  gf <= nfzav;
  aztrwep <= ('W', 'U', '0');
  nfzav <= xo;
end misugmi;



-- Seed after: 13453793971886765492,5983430343285687595
