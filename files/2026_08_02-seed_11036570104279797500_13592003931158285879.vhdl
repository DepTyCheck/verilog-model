-- Seed: 11036570104279797500,13592003931158285879

library ieee;
use ieee.std_logic_1164.all;

entity a is
  port (lbccgjxsz : buffer std_logic_vector(0 downto 4); iw : out std_logic);
end a;

architecture fl of a is
  
begin
  -- Multi-driven assignments
  lbccgjxsz <= "";
  iw <= iw;
  lbccgjxsz <= (others => '0');
end fl;

entity ssyxpvazz is
  port (yxo : inout time);
end ssyxpvazz;

library ieee;
use ieee.std_logic_1164.all;

architecture novahz of ssyxpvazz is
  signal icerj : std_logic;
  signal znvvonim : std_logic;
  signal ywleggqt : std_logic;
  signal l : std_logic_vector(0 downto 4);
begin
  fpes : entity work.a
    port map (lbccgjxsz => l, iw => ywleggqt);
  grwx : entity work.a
    port map (lbccgjxsz => l, iw => znvvonim);
  piahyegi : entity work.a
    port map (lbccgjxsz => l, iw => icerj);
  
  -- Single-driven assignments
  yxo <= 1 sec;
  
  -- Multi-driven assignments
  ywleggqt <= ywleggqt;
end novahz;

library ieee;
use ieee.std_logic_1164.all;

entity zklwem is
  port (pergcv : buffer std_logic_vector(3 to 3));
end zklwem;

library ieee;
use ieee.std_logic_1164.all;

architecture gorbthrsw of zklwem is
  signal lgju : time;
  signal oddl : std_logic;
  signal rmnnjvt : std_logic_vector(0 downto 4);
  signal xckgbljkyv : time;
begin
  nznq : entity work.ssyxpvazz
    port map (yxo => xckgbljkyv);
  g : entity work.a
    port map (lbccgjxsz => rmnnjvt, iw => oddl);
  kzijfhgs : entity work.ssyxpvazz
    port map (yxo => lgju);
  
  -- Multi-driven assignments
  pergcv <= (others => 'X');
  pergcv <= (others => 'L');
  pergcv <= pergcv;
end gorbthrsw;



-- Seed after: 18093113812419723047,13592003931158285879
