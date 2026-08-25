-- Seed: 3059077919774799404,13501862637168280927

library ieee;
use ieee.std_logic_1164.all;

entity ftufech is
  port (powqn : in std_logic_vector(4 to 3); dk : linkage integer; rr : buffer string(5 to 1));
end ftufech;

architecture vq of ftufech is
  
begin
  -- Single-driven assignments
  rr <= rr;
end vq;

library ieee;
use ieee.std_logic_1164.all;

entity z is
  port (clsfwgqv : buffer boolean_vector(3 downto 0); wyeseivcj : linkage std_logic);
end z;

library ieee;
use ieee.std_logic_1164.all;

architecture r of z is
  signal lfzim : string(5 to 1);
  signal wcuwyflg : integer;
  signal cxnbrtqcic : std_logic_vector(4 to 3);
begin
  ibxazwn : entity work.ftufech
    port map (powqn => cxnbrtqcic, dk => wcuwyflg, rr => lfzim);
  
  -- Single-driven assignments
  clsfwgqv <= clsfwgqv;
  
  -- Multi-driven assignments
  cxnbrtqcic <= "";
  cxnbrtqcic <= cxnbrtqcic;
end r;

library ieee;
use ieee.std_logic_1164.all;

entity fsvaow is
  port (hf : out std_logic; ldfmwhf : linkage real);
end fsvaow;

library ieee;
use ieee.std_logic_1164.all;

architecture ferlr of fsvaow is
  signal pvbnkec : std_logic;
  signal tw : boolean_vector(3 downto 0);
  signal u : std_logic;
  signal i : boolean_vector(3 downto 0);
  signal bocvgqq : boolean_vector(3 downto 0);
  signal qmk : boolean_vector(3 downto 0);
begin
  tlpyu : entity work.z
    port map (clsfwgqv => qmk, wyeseivcj => hf);
  gumlvyyiiz : entity work.z
    port map (clsfwgqv => bocvgqq, wyeseivcj => hf);
  uqa : entity work.z
    port map (clsfwgqv => i, wyeseivcj => u);
  offf : entity work.z
    port map (clsfwgqv => tw, wyeseivcj => pvbnkec);
  
  -- Multi-driven assignments
  hf <= 'Z';
end ferlr;



-- Seed after: 5268922791292102126,13501862637168280927
