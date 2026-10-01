-- Seed: 4765666703913485046,15025465285671019065

library ieee;
use ieee.std_logic_1164.all;

entity mmw is
  port (wrpmrp : out bit_vector(1 downto 4); tho : inout integer; oruynnnhw : inout std_logic);
end mmw;

architecture brumrmk of mmw is
  
begin
  -- Single-driven assignments
  tho <= 16#94#;
  wrpmrp <= wrpmrp;
  
  -- Multi-driven assignments
  oruynnnhw <= '-';
  oruynnnhw <= '0';
end brumrmk;

library ieee;
use ieee.std_logic_1164.all;

entity j is
  port (iztbbsseo : inout integer_vector(0 downto 1); qovda : buffer std_logic; emxg : inout integer; m : inout boolean);
end j;

architecture xby of j is
  
begin
  -- Single-driven assignments
  m <= m;
  emxg <= 2#11001#;
  
  -- Multi-driven assignments
  qovda <= '-';
  qovda <= 'U';
end xby;

library ieee;
use ieee.std_logic_1164.all;

entity iqo is
  port (nnjupc : in std_logic; xk : buffer std_logic_vector(3 downto 4));
end iqo;

library ieee;
use ieee.std_logic_1164.all;

architecture sgqbl of iqo is
  signal zyklmubsz : std_logic;
  signal earqbjtwbn : integer;
  signal ajruejvbkj : bit_vector(1 downto 4);
  signal uemgafd : boolean;
  signal gx : integer;
  signal wrjhpvmje : std_logic;
  signal r : integer_vector(0 downto 1);
  signal jhsse : integer;
  signal vqmvzuo : bit_vector(1 downto 4);
  signal brgz : std_logic;
  signal fxmjzbcjfl : integer;
  signal fyltc : bit_vector(1 downto 4);
begin
  yuhlq : entity work.mmw
    port map (wrpmrp => fyltc, tho => fxmjzbcjfl, oruynnnhw => brgz);
  ywmfsqtrc : entity work.mmw
    port map (wrpmrp => vqmvzuo, tho => jhsse, oruynnnhw => brgz);
  nswujjbai : entity work.j
    port map (iztbbsseo => r, qovda => wrjhpvmje, emxg => gx, m => uemgafd);
  jzrbirv : entity work.mmw
    port map (wrpmrp => ajruejvbkj, tho => earqbjtwbn, oruynnnhw => zyklmubsz);
  
  -- Multi-driven assignments
  xk <= "";
  zyklmubsz <= '-';
  wrjhpvmje <= nnjupc;
end sgqbl;



-- Seed after: 11464126488176401190,15025465285671019065
