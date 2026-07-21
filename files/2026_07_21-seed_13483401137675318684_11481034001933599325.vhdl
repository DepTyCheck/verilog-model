-- Seed: 13483401137675318684,11481034001933599325

library ieee;
use ieee.std_logic_1164.all;

entity ridszpf is
  port (x : inout time_vector(0 to 1); wspkbv : linkage std_logic_vector(3 downto 1); zpxornk : in string(4 to 4));
end ridszpf;

architecture te of ridszpf is
  
begin
  -- Single-driven assignments
  x <= (1.0224 ms, 1 ns);
end te;

library ieee;
use ieee.std_logic_1164.all;

entity fq is
  port (p : buffer std_logic; fzvoapflac : linkage std_logic);
end fq;

library ieee;
use ieee.std_logic_1164.all;

architecture btdzdk of fq is
  signal eirdenzbj : time_vector(0 to 1);
  signal muqp : string(4 to 4);
  signal goe : std_logic_vector(3 downto 1);
  signal xrnvwcwbja : time_vector(0 to 1);
  signal mhxlopcmic : string(4 to 4);
  signal vvfskvl : std_logic_vector(3 downto 1);
  signal unfkhkld : time_vector(0 to 1);
begin
  l : entity work.ridszpf
    port map (x => unfkhkld, wspkbv => vvfskvl, zpxornk => mhxlopcmic);
  nyvnwwtnkb : entity work.ridszpf
    port map (x => xrnvwcwbja, wspkbv => goe, zpxornk => muqp);
  zdxlmsbnh : entity work.ridszpf
    port map (x => eirdenzbj, wspkbv => vvfskvl, zpxornk => muqp);
  
  -- Single-driven assignments
  mhxlopcmic <= (others => 'a');
  
  -- Multi-driven assignments
  p <= p;
  vvfskvl <= vvfskvl;
  p <= 'X';
  vvfskvl <= "W0Z";
end btdzdk;



-- Seed after: 11263178782207320439,11481034001933599325
