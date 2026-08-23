-- Seed: 8324049755101167712,4245627776430562977

entity h is
  port (ilzctf : linkage boolean_vector(3 to 3); mhpijwiwx : in real);
end h;

architecture ln of h is
  
begin
  
end ln;

entity wqvwjch is
  port (wm : buffer real; zxnybp : in real);
end wqvwjch;

architecture emywxrxvmb of wqvwjch is
  signal lukyycnha : real;
  signal fcgxx : boolean_vector(3 to 3);
begin
  vgb : entity work.h
    port map (ilzctf => fcgxx, mhpijwiwx => lukyycnha);
  
  -- Single-driven assignments
  wm <= 4.3_4_3;
  lukyycnha <= 16#5_F_D_7_C.F7#;
end emywxrxvmb;

library ieee;
use ieee.std_logic_1164.all;

entity isn is
  port (emvhp : out real; xcm : buffer std_logic_vector(1 downto 0));
end isn;

architecture ntakwfyjna of isn is
  signal w : real;
  signal iu : boolean_vector(3 to 3);
  signal oymmobtqqt : boolean_vector(3 to 3);
  signal csyumkiav : real;
  signal ifm : boolean_vector(3 to 3);
  signal hehi : real;
begin
  uaninikaj : entity work.wqvwjch
    port map (wm => emvhp, zxnybp => hehi);
  nbuwdkcwvb : entity work.h
    port map (ilzctf => ifm, mhpijwiwx => csyumkiav);
  ssywmtx : entity work.h
    port map (ilzctf => oymmobtqqt, mhpijwiwx => emvhp);
  oqfnvf : entity work.h
    port map (ilzctf => iu, mhpijwiwx => w);
  
  -- Single-driven assignments
  hehi <= 103.34;
  csyumkiav <= emvhp;
  w <= 2_2_1.2_1_3_3;
  
  -- Multi-driven assignments
  xcm <= xcm;
  xcm <= ('W', 'L');
  xcm <= xcm;
  xcm <= xcm;
end ntakwfyjna;



-- Seed after: 17860378240723663223,4245627776430562977
