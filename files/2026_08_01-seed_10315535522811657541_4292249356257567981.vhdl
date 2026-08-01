-- Seed: 10315535522811657541,4292249356257567981

library ieee;
use ieee.std_logic_1164.all;

entity lj is
  port (id : in time; bvkvr : buffer time; wyixpk : out std_logic_vector(2 to 2); irbyxrqbp : out real);
end lj;

architecture mdxrltoc of lj is
  
begin
  -- Single-driven assignments
  irbyxrqbp <= 44314.1;
  bvkvr <= 0 min;
  
  -- Multi-driven assignments
  wyixpk <= wyixpk;
  wyixpk <= (others => 'L');
  wyixpk <= wyixpk;
end mdxrltoc;

entity ksa is
  port (hxntgwepg : inout real; qjveu : out bit);
end ksa;

library ieee;
use ieee.std_logic_1164.all;

architecture polnv of ksa is
  signal fs : real;
  signal qphedwyb : std_logic_vector(2 to 2);
  signal z : std_logic_vector(2 to 2);
  signal k : time;
  signal rawv : time;
begin
  oypbmvldcy : entity work.lj
    port map (id => rawv, bvkvr => k, wyixpk => z, irbyxrqbp => hxntgwepg);
  mqihc : entity work.lj
    port map (id => k, bvkvr => rawv, wyixpk => qphedwyb, irbyxrqbp => fs);
end polnv;



-- Seed after: 2664656050792948218,4292249356257567981
