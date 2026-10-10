-- Seed: 9285507947849005641,511364357853360275

library ieee;
use ieee.std_logic_1164.all;

entity ts is
  port (xrzgd : out std_logic_vector(0 downto 4); lwqqppnyey : buffer std_logic; vukfae : linkage bit);
end ts;

architecture fyspdbbqyt of ts is
  
begin
  -- Multi-driven assignments
  lwqqppnyey <= '1';
end fyspdbbqyt;

library ieee;
use ieee.std_logic_1164.all;

entity tixittol is
  port (bc : in std_logic);
end tixittol;

library ieee;
use ieee.std_logic_1164.all;

architecture cecmv of tixittol is
  signal phxpmmo : bit;
  signal xacaomfa : std_logic_vector(0 downto 4);
  signal damscrerx : bit;
  signal qfjrbras : std_logic_vector(0 downto 4);
  signal zgguteouhr : bit;
  signal xumr : std_logic;
  signal ykengggkja : std_logic_vector(0 downto 4);
begin
  vqnm : entity work.ts
    port map (xrzgd => ykengggkja, lwqqppnyey => xumr, vukfae => zgguteouhr);
  gir : entity work.ts
    port map (xrzgd => qfjrbras, lwqqppnyey => xumr, vukfae => damscrerx);
  z : entity work.ts
    port map (xrzgd => xacaomfa, lwqqppnyey => xumr, vukfae => phxpmmo);
  
  -- Multi-driven assignments
  qfjrbras <= "";
  ykengggkja <= (others => '0');
end cecmv;



-- Seed after: 8732347770281545990,511364357853360275
