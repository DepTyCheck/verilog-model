-- Seed: 1165349570039718515,3400751927341804175

library ieee;
use ieee.std_logic_1164.all;

entity vtbeeft is
  port (m : inout time; suwzfztt : in string(5 downto 1); acymdvryt : in std_logic; cfberbvdff : out std_logic_vector(4 to 2));
end vtbeeft;

architecture zirsz of vtbeeft is
  
begin
  -- Multi-driven assignments
  cfberbvdff <= "";
end zirsz;

library ieee;
use ieee.std_logic_1164.all;

entity gsor is
  port ( e : linkage bit_vector(4 downto 1)
  ; kvw : buffer string(4 to 4)
  ; uyicx : in std_logic_vector(1 downto 4)
  ; otknv : linkage std_logic_vector(3 to 4)
  );
end gsor;

library ieee;
use ieee.std_logic_1164.all;

architecture kc of gsor is
  signal nnqosd : std_logic_vector(4 to 2);
  signal qlkft : time;
  signal jvdiztxn : std_logic_vector(4 to 2);
  signal ibzthz : string(5 downto 1);
  signal bzxmzsakq : time;
  signal pjmeacq : std_logic_vector(4 to 2);
  signal msrryx : std_logic;
  signal nflt : string(5 downto 1);
  signal c : time;
begin
  ceags : entity work.vtbeeft
    port map (m => c, suwzfztt => nflt, acymdvryt => msrryx, cfberbvdff => pjmeacq);
  by : entity work.vtbeeft
    port map (m => bzxmzsakq, suwzfztt => ibzthz, acymdvryt => msrryx, cfberbvdff => jvdiztxn);
  khx : entity work.vtbeeft
    port map (m => qlkft, suwzfztt => nflt, acymdvryt => msrryx, cfberbvdff => nnqosd);
  
  -- Single-driven assignments
  kvw <= "y";
  ibzthz <= "smivp";
  
  -- Multi-driven assignments
  jvdiztxn <= "";
  pjmeacq <= (others => '0');
end kc;



-- Seed after: 9483108322053525533,3400751927341804175
