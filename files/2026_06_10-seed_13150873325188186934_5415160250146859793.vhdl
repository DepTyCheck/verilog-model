-- Seed: 13150873325188186934,5415160250146859793

library ieee;
use ieee.std_logic_1164.all;

entity akwdmcc is
  port (lorvta : in bit; vo : out std_logic_vector(0 downto 2));
end akwdmcc;



architecture ddaslr of akwdmcc is
  
begin
  
end ddaslr;

library ieee;
use ieee.std_logic_1164.all;

entity lilleel is
  port (bzsmmtq : inout std_logic_vector(1 to 1); ymgzvd : in bit_vector(0 to 2));
end lilleel;



architecture iaczhvt of lilleel is
  
begin
  
end iaczhvt;

library ieee;
use ieee.std_logic_1164.all;

entity holkk is
  port (pkh : in character; cgy : out real; tcolegwh : in std_logic_vector(4 to 3));
end holkk;

library ieee;
use ieee.std_logic_1164.all;

architecture bhruut of holkk is
  signal ikkszi : bit;
  signal iosrct : bit_vector(0 to 2);
  signal jjkbhqald : std_logic_vector(1 to 1);
  signal uidkazf : bit_vector(0 to 2);
  signal plgeqdd : std_logic_vector(1 to 1);
  signal crx : std_logic_vector(0 downto 2);
  signal mbsi : bit;
begin
  xoy : entity work.akwdmcc
    port map (lorvta => mbsi, vo => crx);
  nqk : entity work.lilleel
    port map (bzsmmtq => plgeqdd, ymgzvd => uidkazf);
  fvt : entity work.lilleel
    port map (bzsmmtq => jjkbhqald, ymgzvd => iosrct);
  psv : entity work.akwdmcc
    port map (lorvta => ikkszi, vo => crx);
end bhruut;

library ieee;
use ieee.std_logic_1164.all;

entity oekazwt is
  port (rwewhip : linkage integer; mhvdfsmasw : inout std_logic_vector(0 downto 3); iymt : inout real);
end oekazwt;

library ieee;
use ieee.std_logic_1164.all;

architecture qadew of oekazwt is
  signal dksjoburyv : bit_vector(0 to 2);
  signal eq : std_logic_vector(1 to 1);
  signal u : std_logic_vector(0 downto 2);
  signal iyktoise : bit;
  signal xkynlvssdv : real;
  signal dopxtbqjzs : character;
begin
  vuw : entity work.holkk
    port map (pkh => dopxtbqjzs, cgy => xkynlvssdv, tcolegwh => mhvdfsmasw);
  v : entity work.akwdmcc
    port map (lorvta => iyktoise, vo => u);
  erbadq : entity work.lilleel
    port map (bzsmmtq => eq, ymgzvd => dksjoburyv);
  bnwjnjyuds : entity work.akwdmcc
    port map (lorvta => iyktoise, vo => mhvdfsmasw);
end qadew;



-- Seed after: 11593043141486437450,5415160250146859793
