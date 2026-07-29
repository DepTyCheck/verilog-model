-- Seed: 11570959574662507527,14641901754878719179

library ieee;
use ieee.std_logic_1164.all;

entity a is
  port (rh : buffer std_logic);
end a;

architecture t of a is
  
begin
  -- Multi-driven assignments
  rh <= rh;
end t;

entity btpjesrre is
  port (iztzmrzp : buffer string(2 downto 2));
end btpjesrre;

library ieee;
use ieee.std_logic_1164.all;

architecture iv of btpjesrre is
  signal jz : std_logic;
begin
  odobru : entity work.a
    port map (rh => jz);
  qwojaq : entity work.a
    port map (rh => jz);
  
  -- Single-driven assignments
  iztzmrzp <= iztzmrzp;
  
  -- Multi-driven assignments
  jz <= 'W';
  jz <= 'W';
  jz <= 'X';
end iv;

entity siehv is
  port (tswugsin : out integer_vector(0 downto 1); ueudnh : buffer time);
end siehv;

library ieee;
use ieee.std_logic_1164.all;

architecture jak of siehv is
  signal zjognexr : string(2 downto 2);
  signal afq : std_logic;
  signal jqetzhour : std_logic;
begin
  quahndrk : entity work.a
    port map (rh => jqetzhour);
  sxx : entity work.a
    port map (rh => jqetzhour);
  kumcxv : entity work.a
    port map (rh => afq);
  ug : entity work.btpjesrre
    port map (iztzmrzp => zjognexr);
  
  -- Single-driven assignments
  ueudnh <= 8#5_0_7# us;
  tswugsin <= tswugsin;
  
  -- Multi-driven assignments
  afq <= jqetzhour;
  afq <= 'Z';
end jak;



-- Seed after: 10155860960750245566,14641901754878719179
