-- Seed: 13827453821523311956,10871023049702252113

library ieee;
use ieee.std_logic_1164.all;

entity aj is
  port (fbhezluygc : in integer_vector(1 to 2); uzogfxasvq : buffer std_logic);
end aj;

architecture iymoyjmzvg of aj is
  
begin
  -- Multi-driven assignments
  uzogfxasvq <= uzogfxasvq;
  uzogfxasvq <= 'L';
  uzogfxasvq <= '-';
  uzogfxasvq <= 'H';
end iymoyjmzvg;

entity kmjaxnyncu is
  port (cnkyscoys : buffer boolean_vector(2 downto 0));
end kmjaxnyncu;

library ieee;
use ieee.std_logic_1164.all;

architecture ukpj of kmjaxnyncu is
  signal jqqjfaonp : std_logic;
  signal u : integer_vector(1 to 2);
  signal nztxi : std_logic;
  signal zfcnpztwx : integer_vector(1 to 2);
begin
  mmvtsnu : entity work.aj
    port map (fbhezluygc => zfcnpztwx, uzogfxasvq => nztxi);
  batxjgzm : entity work.aj
    port map (fbhezluygc => u, uzogfxasvq => jqqjfaonp);
  
  -- Single-driven assignments
  u <= zfcnpztwx;
  cnkyscoys <= cnkyscoys;
  
  -- Multi-driven assignments
  nztxi <= '-';
end ukpj;



-- Seed after: 5948843696005143887,10871023049702252113
