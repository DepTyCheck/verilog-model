-- Seed: 17482544541599089013,18037650846010261179

entity wkp is
  port (mqvu : in boolean);
end wkp;

architecture gesffltqg of wkp is
  
begin
  
end gesffltqg;

library ieee;
use ieee.std_logic_1164.all;

entity fcqfhkmkk is
  port ( rhlelutgwu : in string(4 to 5)
  ; nsgoq : linkage std_logic_vector(4 downto 2)
  ; jlvyogycfn : in real_vector(0 downto 2)
  ; txdajmew : linkage std_logic_vector(3 to 0)
  );
end fcqfhkmkk;

architecture ahz of fcqfhkmkk is
  
begin
  
end ahz;

library ieee;
use ieee.std_logic_1164.all;

entity vcbmggdf is
  port (eqnoqbouec : in std_logic_vector(1 to 2); etaz : out time; z : linkage std_logic);
end vcbmggdf;

architecture kvbnr of vcbmggdf is
  signal m : boolean;
begin
  lsovizma : entity work.wkp
    port map (mqvu => m);
  
  -- Single-driven assignments
  etaz <= 8#32# ms;
  m <= m;
end kvbnr;

entity ydkeisfsk is
  port (qx : buffer time; hr : out integer_vector(2 to 2); fgmhqhzy : inout integer; bsev : out severity_level);
end ydkeisfsk;

library ieee;
use ieee.std_logic_1164.all;

architecture qwhplmyayx of ydkeisfsk is
  signal dthbxu : std_logic;
  signal dbynovg : std_logic_vector(1 to 2);
  signal xzgdkw : std_logic;
  signal zladdlg : time;
  signal zddy : std_logic;
  signal lxxvmon : time;
  signal tjdz : std_logic_vector(1 to 2);
  signal kb : boolean;
begin
  pknqe : entity work.wkp
    port map (mqvu => kb);
  nymbwc : entity work.vcbmggdf
    port map (eqnoqbouec => tjdz, etaz => lxxvmon, z => zddy);
  fndwo : entity work.vcbmggdf
    port map (eqnoqbouec => tjdz, etaz => zladdlg, z => xzgdkw);
  xk : entity work.vcbmggdf
    port map (eqnoqbouec => dbynovg, etaz => qx, z => dthbxu);
  
  -- Multi-driven assignments
  dbynovg <= ('-', 'W');
  dbynovg <= ('Z', 'W');
end qwhplmyayx;



-- Seed after: 2551504633089424984,18037650846010261179
