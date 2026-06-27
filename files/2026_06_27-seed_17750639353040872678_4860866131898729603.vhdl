-- Seed: 17750639353040872678,4860866131898729603

entity sdmf is
  port (ztrmy : buffer integer_vector(3 downto 4));
end sdmf;

architecture nzgmr of sdmf is
  
begin
  -- Single-driven assignments
  ztrmy <= (others => 0);
end nzgmr;

library ieee;
use ieee.std_logic_1164.all;

entity jge is
  port (lsvrimvjn : in std_logic_vector(0 to 2); uv : in real; hkzzm : in integer; xrabxz : linkage real);
end jge;

architecture ooenu of jge is
  signal aghx : integer_vector(3 downto 4);
  signal lxkaownre : integer_vector(3 downto 4);
begin
  szcejn : entity work.sdmf
    port map (ztrmy => lxkaownre);
  oxgokg : entity work.sdmf
    port map (ztrmy => aghx);
end ooenu;

library ieee;
use ieee.std_logic_1164.all;

entity srewzwze is
  port (tn : in std_logic; bgqumsgyhu : linkage real; jycdu : linkage integer_vector(1 downto 0));
end srewzwze;

library ieee;
use ieee.std_logic_1164.all;

architecture pgsjkkbg of srewzwze is
  signal i : integer_vector(3 downto 4);
  signal muwyugbfai : integer;
  signal xeaikcri : real;
  signal poxm : std_logic_vector(0 to 2);
  signal g : integer_vector(3 downto 4);
  signal p : integer_vector(3 downto 4);
begin
  aibmazorqq : entity work.sdmf
    port map (ztrmy => p);
  driydz : entity work.sdmf
    port map (ztrmy => g);
  haq : entity work.jge
    port map (lsvrimvjn => poxm, uv => xeaikcri, hkzzm => muwyugbfai, xrabxz => xeaikcri);
  fuci : entity work.sdmf
    port map (ztrmy => i);
  
  -- Multi-driven assignments
  poxm <= ('-', 'L', 'H');
  poxm <= ('0', 'Z', 'L');
  poxm <= "W0Z";
end pgsjkkbg;



-- Seed after: 17923464739109291300,4860866131898729603
