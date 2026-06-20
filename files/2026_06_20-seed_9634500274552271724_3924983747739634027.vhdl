-- Seed: 9634500274552271724,3924983747739634027

entity jxpt is
  port (pjdwsic : inout integer; bnhtrkmq : buffer real);
end jxpt;

architecture mitwxhunqn of jxpt is
  
begin
  -- Single-driven assignments
  pjdwsic <= 16#E322#;
end mitwxhunqn;

library ieee;
use ieee.std_logic_1164.all;

entity y is
  port (d : in std_logic_vector(2 downto 2); e : in bit; bqkr : inout real; zmwjos : out bit);
end y;

architecture xxm of y is
  signal jnceaehf : integer;
  signal jgf : real;
  signal ggbuvs : integer;
  signal khl : real;
  signal cpupqgcz : integer;
begin
  dxecjvf : entity work.jxpt
    port map (pjdwsic => cpupqgcz, bnhtrkmq => khl);
  hwqtevcrbk : entity work.jxpt
    port map (pjdwsic => ggbuvs, bnhtrkmq => jgf);
  clxquq : entity work.jxpt
    port map (pjdwsic => jnceaehf, bnhtrkmq => bqkr);
  
  -- Single-driven assignments
  zmwjos <= '0';
end xxm;

entity heejdcc is
  port (mtwamptboe : buffer time; ef : out time; pfq : buffer bit_vector(2 to 3));
end heejdcc;

library ieee;
use ieee.std_logic_1164.all;

architecture pl of heejdcc is
  signal bw : real;
  signal xn : bit;
  signal ml : std_logic_vector(2 downto 2);
begin
  k : entity work.y
    port map (d => ml, e => xn, bqkr => bw, zmwjos => xn);
  
  -- Single-driven assignments
  pfq <= ('0', '1');
  mtwamptboe <= 1 hr;
  ef <= 8#2# ns;
  
  -- Multi-driven assignments
  ml <= "U";
  ml <= (others => 'U');
  ml <= (others => 'W');
  ml <= "Z";
end pl;



-- Seed after: 9268652836898985028,3924983747739634027
