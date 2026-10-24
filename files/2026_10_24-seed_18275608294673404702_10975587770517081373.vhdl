-- Seed: 18275608294673404702,10975587770517081373

library ieee;
use ieee.std_logic_1164.all;

entity bfkql is
  port (eqtutzn : inout std_logic);
end bfkql;

architecture ry of bfkql is
  
begin
  -- Multi-driven assignments
  eqtutzn <= eqtutzn;
  eqtutzn <= eqtutzn;
  eqtutzn <= eqtutzn;
end ry;

entity zbzovf is
  port (xipxbpcbd : out time; ydyny : out time);
end zbzovf;

library ieee;
use ieee.std_logic_1164.all;

architecture cbjymh of zbzovf is
  signal zwqpxfp : std_logic;
  signal ykufplu : std_logic;
begin
  auz : entity work.bfkql
    port map (eqtutzn => ykufplu);
  nppqhoabf : entity work.bfkql
    port map (eqtutzn => ykufplu);
  pjc : entity work.bfkql
    port map (eqtutzn => zwqpxfp);
  
  -- Single-driven assignments
  ydyny <= 0.40 ns;
  xipxbpcbd <= 16#9_0_2.4_7_3_C# fs;
  
  -- Multi-driven assignments
  ykufplu <= 'L';
  ykufplu <= '-';
end cbjymh;

entity kzoke is
  port (pver : linkage bit; iu : buffer integer);
end kzoke;

library ieee;
use ieee.std_logic_1164.all;

architecture xoxajbkwx of kzoke is
  signal asn : time;
  signal po : time;
  signal ziuvxhyz : std_logic;
begin
  rexdimbu : entity work.bfkql
    port map (eqtutzn => ziuvxhyz);
  faf : entity work.bfkql
    port map (eqtutzn => ziuvxhyz);
  x : entity work.zbzovf
    port map (xipxbpcbd => po, ydyny => asn);
  
  -- Multi-driven assignments
  ziuvxhyz <= ziuvxhyz;
end xoxajbkwx;

entity zcugc is
  port (arv : inout time);
end zcugc;

architecture swxma of zcugc is
  signal vuuvjyb : integer;
  signal qufhxnzidf : bit;
  signal zmyerqyyve : time;
  signal tgezm : time;
begin
  d : entity work.zbzovf
    port map (xipxbpcbd => tgezm, ydyny => zmyerqyyve);
  yfghzcrbab : entity work.kzoke
    port map (pver => qufhxnzidf, iu => vuuvjyb);
  
  -- Single-driven assignments
  arv <= 8#7_1_1_4_2# fs;
end swxma;



-- Seed after: 3533333375248083993,10975587770517081373
