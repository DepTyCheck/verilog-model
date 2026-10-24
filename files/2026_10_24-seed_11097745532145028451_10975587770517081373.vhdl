-- Seed: 11097745532145028451,10975587770517081373

entity ubkylc is
  port (p : in real; aqf : buffer real);
end ubkylc;

architecture fjarqmyjjk of ubkylc is
  
begin
  
end fjarqmyjjk;

library ieee;
use ieee.std_logic_1164.all;

entity g is
  port (lwxix : linkage integer; ndymj : out std_logic);
end g;

architecture awqhtxvwxx of g is
  signal jycdbsx : real;
  signal j : real;
begin
  yzrd : entity work.ubkylc
    port map (p => j, aqf => jycdbsx);
  rmtfk : entity work.ubkylc
    port map (p => jycdbsx, aqf => j);
  
  -- Multi-driven assignments
  ndymj <= ndymj;
  ndymj <= '-';
  ndymj <= ndymj;
  ndymj <= 'W';
end awqhtxvwxx;



-- Seed after: 13748920427682984180,10975587770517081373
