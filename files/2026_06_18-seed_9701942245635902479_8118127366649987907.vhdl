-- Seed: 9701942245635902479,8118127366649987907

library ieee;
use ieee.std_logic_1164.all;

entity gljtwu is
  port (qwcgnfnk : inout std_logic_vector(3 downto 2); x : out real; c : buffer integer);
end gljtwu;

architecture ibiwgvpdw of gljtwu is
  
begin
  -- Single-driven assignments
  x <= 8#2_6_3_7_3.2_0#;
  c <= 2#0_0_1_0_0#;
  
  -- Multi-driven assignments
  qwcgnfnk <= ('L', 'H');
  qwcgnfnk <= "1H";
  qwcgnfnk <= ('-', 'Z');
  qwcgnfnk <= "-U";
end ibiwgvpdw;

entity lzzaxob is
  port (c : buffer boolean_vector(0 downto 3); e : inout integer; fab : buffer real);
end lzzaxob;

architecture fupe of lzzaxob is
  
begin
  -- Single-driven assignments
  fab <= 2#0101.0#;
  e <= 8#1_5_6_1_3#;
  c <= (others => TRUE);
end fupe;

entity gdc is
  port (dthuqrcuus : inout integer; kgiltifi : out integer; jiwfrtlom : out bit_vector(1 downto 2));
end gdc;

library ieee;
use ieee.std_logic_1164.all;

architecture tec of gdc is
  signal t : real;
  signal fywwjbq : std_logic_vector(3 downto 2);
  signal volerieedf : real;
  signal cskevk : boolean_vector(0 downto 3);
  signal gczpcrvbxb : real;
  signal gjjczrdk : integer;
  signal wot : boolean_vector(0 downto 3);
begin
  hnmehrq : entity work.lzzaxob
    port map (c => wot, e => gjjczrdk, fab => gczpcrvbxb);
  kuk : entity work.lzzaxob
    port map (c => cskevk, e => kgiltifi, fab => volerieedf);
  apj : entity work.gljtwu
    port map (qwcgnfnk => fywwjbq, x => t, c => dthuqrcuus);
  
  -- Multi-driven assignments
  fywwjbq <= "XL";
  fywwjbq <= ('H', 'X');
  fywwjbq <= "0Z";
  fywwjbq <= "0X";
end tec;

entity qaaa is
  port (bfhncdyh : linkage time_vector(2 downto 4); vplyp : out boolean_vector(0 to 3); sdol : inout time);
end qaaa;

architecture ykzptgedhr of qaaa is
  signal zca : real;
  signal zdbeylp : integer;
  signal zidshfa : boolean_vector(0 downto 3);
begin
  sps : entity work.lzzaxob
    port map (c => zidshfa, e => zdbeylp, fab => zca);
  
  -- Single-driven assignments
  sdol <= 0 hr;
  vplyp <= (FALSE, TRUE, FALSE, FALSE);
end ykzptgedhr;



-- Seed after: 16377044512434974734,8118127366649987907
