-- Seed: 454376742738889090,14652815260262078753

library ieee;
use ieee.std_logic_1164.all;

entity ii is
  port (tpamb : in integer; rae : inout std_logic_vector(2 downto 2); uwfsq : buffer std_logic_vector(4 downto 2); bz : linkage time);
end ii;

architecture ghwwvs of ii is
  
begin
  -- Multi-driven assignments
  uwfsq <= ('0', 'L', '-');
  rae <= (others => 'Z');
  uwfsq <= ('-', '1', '1');
  uwfsq <= ('X', '-', 'Z');
end ghwwvs;

library ieee;
use ieee.std_logic_1164.all;

entity ecb is
  port (azao : linkage time; uumv : linkage std_logic_vector(1 to 0); ytc : linkage real);
end ecb;

library ieee;
use ieee.std_logic_1164.all;

architecture c of ecb is
  signal jgaraj : time;
  signal gpilqf : std_logic_vector(4 downto 2);
  signal ulod : std_logic_vector(2 downto 2);
  signal jijai : integer;
begin
  xlkkq : entity work.ii
    port map (tpamb => jijai, rae => ulod, uwfsq => gpilqf, bz => jgaraj);
  
  -- Single-driven assignments
  jijai <= 3_3_2_4_4;
  
  -- Multi-driven assignments
  ulod <= (others => 'W');
end c;

library ieee;
use ieee.std_logic_1164.all;

entity winnzq is
  port (rffdxrz : buffer bit; c : linkage std_logic; w : out real);
end winnzq;

library ieee;
use ieee.std_logic_1164.all;

architecture yrvmy of winnzq is
  signal ffpkcpvi : time;
  signal tsoqeviy : std_logic_vector(4 downto 2);
  signal ceftzlxyaq : std_logic_vector(2 downto 2);
  signal ztctcorajf : integer;
  signal tzsxnmjf : real;
  signal ewhxomjibt : time;
  signal yoooyizg : real;
  signal fsarxp : std_logic_vector(1 to 0);
  signal xzqdkdzcdp : time;
begin
  tpqmci : entity work.ecb
    port map (azao => xzqdkdzcdp, uumv => fsarxp, ytc => yoooyizg);
  hhzuctvr : entity work.ecb
    port map (azao => ewhxomjibt, uumv => fsarxp, ytc => tzsxnmjf);
  atv : entity work.ii
    port map (tpamb => ztctcorajf, rae => ceftzlxyaq, uwfsq => tsoqeviy, bz => ffpkcpvi);
  
  -- Single-driven assignments
  ztctcorajf <= 8#30752#;
  rffdxrz <= '1';
  w <= 16#7.E_6_F_8_B#;
  
  -- Multi-driven assignments
  fsarxp <= "";
  fsarxp <= (others => '0');
end yrvmy;



-- Seed after: 2412142731319086362,14652815260262078753
