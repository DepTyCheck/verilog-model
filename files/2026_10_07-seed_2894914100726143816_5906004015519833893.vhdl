-- Seed: 2894914100726143816,5906004015519833893

entity f is
  port (egkvlpatyp : linkage real; ccc : in bit_vector(2 to 0); axvuyw : inout bit_vector(0 downto 1));
end f;

architecture gnx of f is
  
begin
  -- Single-driven assignments
  axvuyw <= (others => '0');
end gnx;

library ieee;
use ieee.std_logic_1164.all;

entity oehxvjvc is
  port (nbvze : in std_logic_vector(4 downto 4));
end oehxvjvc;

architecture ox of oehxvjvc is
  signal kl : bit_vector(0 downto 1);
  signal fbagmuwzn : real;
  signal lcyxyp : bit_vector(0 downto 1);
  signal wtqcwq : real;
  signal ycmt : bit_vector(2 to 0);
  signal j : real;
begin
  dc : entity work.f
    port map (egkvlpatyp => j, ccc => ycmt, axvuyw => ycmt);
  rj : entity work.f
    port map (egkvlpatyp => wtqcwq, ccc => ycmt, axvuyw => lcyxyp);
  gx : entity work.f
    port map (egkvlpatyp => fbagmuwzn, ccc => kl, axvuyw => kl);
end ox;

entity wopay is
  port (dyhpfaqekq : inout integer);
end wopay;

library ieee;
use ieee.std_logic_1164.all;

architecture fb of wopay is
  signal gddeksy : bit_vector(0 downto 1);
  signal se : real;
  signal dqpumjil : std_logic_vector(4 downto 4);
  signal xtsoh : bit_vector(2 to 0);
  signal wh : real;
  signal jwytzpc : bit_vector(0 downto 1);
  signal ylayowe : real;
begin
  zs : entity work.f
    port map (egkvlpatyp => ylayowe, ccc => jwytzpc, axvuyw => jwytzpc);
  z : entity work.f
    port map (egkvlpatyp => wh, ccc => xtsoh, axvuyw => xtsoh);
  yeoyim : entity work.oehxvjvc
    port map (nbvze => dqpumjil);
  jqsrhk : entity work.f
    port map (egkvlpatyp => se, ccc => xtsoh, axvuyw => gddeksy);
  
  -- Single-driven assignments
  dyhpfaqekq <= 16#1_7#;
  
  -- Multi-driven assignments
  dqpumjil <= dqpumjil;
  dqpumjil <= "0";
  dqpumjil <= dqpumjil;
end fb;



-- Seed after: 2756387579725141513,5906004015519833893
