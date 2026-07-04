-- Seed: 4102032252629359334,6290177331721581829

entity hakualree is
  port (c : out boolean_vector(2 to 0); indvq : in time);
end hakualree;

architecture w of hakualree is
  
begin
  -- Single-driven assignments
  c <= (others => TRUE);
end w;

use std.reflection.all;

entity c is
  port (nmnbtda : inout record_value_mirror);
end c;

architecture ydzxkp of c is
  signal jk : boolean_vector(2 to 0);
  signal tz : boolean_vector(2 to 0);
  signal s : time;
  signal xoa : boolean_vector(2 to 0);
begin
  kxlpuuan : entity work.hakualree
    port map (c => xoa, indvq => s);
  sfmwngyx : entity work.hakualree
    port map (c => tz, indvq => s);
  rqtb : entity work.hakualree
    port map (c => jk, indvq => s);
  
  -- Single-driven assignments
  s <= 10.4 ms;
end ydzxkp;



-- Seed after: 10913052319601105603,6290177331721581829
