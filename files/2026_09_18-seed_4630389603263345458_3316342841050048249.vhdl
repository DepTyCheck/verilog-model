-- Seed: 4630389603263345458,3316342841050048249

entity f is
  port (d : buffer integer_vector(0 to 1); y : inout character; aku : out real);
end f;

architecture yxncxyxuta of f is
  
begin
  -- Single-driven assignments
  aku <= aku;
end yxncxyxuta;

library ieee;
use ieee.std_logic_1164.all;

entity v is
  port (ujgc : inout std_logic; rcgcsbkg : out std_logic; ksiqybzsav : inout integer_vector(1 downto 4));
end v;

architecture llpbszwrw of v is
  
begin
  -- Single-driven assignments
  ksiqybzsav <= (others => 0);
end llpbszwrw;

entity hikj is
  port (op : inout integer; bhv : out integer; shk : inout integer);
end hikj;

library ieee;
use ieee.std_logic_1164.all;

architecture ryrmgblteq of hikj is
  signal sb : real;
  signal juhpornd : character;
  signal siahj : integer_vector(0 to 1);
  signal e : integer_vector(1 downto 4);
  signal dnyetb : std_logic;
  signal egazmvmiz : integer_vector(1 downto 4);
  signal r : std_logic;
begin
  b : entity work.v
    port map (ujgc => r, rcgcsbkg => r, ksiqybzsav => egazmvmiz);
  ouhpj : entity work.v
    port map (ujgc => r, rcgcsbkg => dnyetb, ksiqybzsav => e);
  svuq : entity work.f
    port map (d => siahj, y => juhpornd, aku => sb);
  
  -- Single-driven assignments
  op <= bhv;
  
  -- Multi-driven assignments
  r <= '-';
end ryrmgblteq;



-- Seed after: 12677308785869300726,3316342841050048249
