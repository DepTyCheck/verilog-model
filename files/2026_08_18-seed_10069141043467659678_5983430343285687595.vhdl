-- Seed: 10069141043467659678,5983430343285687595

library ieee;
use ieee.std_logic_1164.all;

entity siuxfqxbo is
  port (pnxpbhjh : linkage std_logic_vector(0 to 3); brlnigygu : linkage std_logic_vector(1 downto 4));
end siuxfqxbo;

architecture u of siuxfqxbo is
  
begin
  
end u;

entity l is
  port (joa : inout real; ayxlnprut : buffer real);
end l;

library ieee;
use ieee.std_logic_1164.all;

architecture eouzb of l is
  signal dlpohcmmh : std_logic_vector(1 downto 4);
  signal likosfnhw : std_logic_vector(1 downto 4);
  signal ffhpfel : std_logic_vector(0 to 3);
begin
  xgfgxgq : entity work.siuxfqxbo
    port map (pnxpbhjh => ffhpfel, brlnigygu => likosfnhw);
  gcdo : entity work.siuxfqxbo
    port map (pnxpbhjh => ffhpfel, brlnigygu => dlpohcmmh);
  
  -- Single-driven assignments
  ayxlnprut <= 16#5_F_C.0_1_6_E_9#;
  joa <= 16#0_2_7_B.B_4_0_A_6#;
  
  -- Multi-driven assignments
  ffhpfel <= "H0XX";
  ffhpfel <= ffhpfel;
  ffhpfel <= ('X', 'L', 'W', 'X');
  ffhpfel <= ffhpfel;
end eouzb;



-- Seed after: 17700752559574663181,5983430343285687595
