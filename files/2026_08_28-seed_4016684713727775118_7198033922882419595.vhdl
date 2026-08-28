-- Seed: 4016684713727775118,7198033922882419595

library ieee;
use ieee.std_logic_1164.all;

entity hqpynxo is
  port (ahx : inout severity_level; batsibh : buffer std_logic_vector(2 to 3); watf : out real_vector(2 to 0); mxrka : in std_logic);
end hqpynxo;

architecture eo of hqpynxo is
  
begin
  -- Single-driven assignments
  watf <= (others => 0.0);
  ahx <= ahx;
  
  -- Multi-driven assignments
  batsibh <= batsibh;
  batsibh <= "01";
end eo;

library ieee;
use ieee.std_logic_1164.all;

entity xhsvp is
  port (vxka : out severity_level; izczfy : buffer std_logic; vegwjcuak : in std_logic_vector(0 to 1); xetto : in real);
end xhsvp;

library ieee;
use ieee.std_logic_1164.all;

architecture ljggcq of xhsvp is
  signal fhoepdnbla : real_vector(2 to 0);
  signal noj : std_logic_vector(2 to 3);
  signal rngksb : severity_level;
begin
  rd : entity work.hqpynxo
    port map (ahx => rngksb, batsibh => noj, watf => fhoepdnbla, mxrka => izczfy);
  
  -- Multi-driven assignments
  izczfy <= '0';
  izczfy <= 'H';
end ljggcq;

library ieee;
use ieee.std_logic_1164.all;

entity sbo is
  port (zsn : linkage std_logic; dzjrdbdge : inout std_logic_vector(0 downto 3));
end sbo;

library ieee;
use ieee.std_logic_1164.all;

architecture gf of sbo is
  signal vkqbjfl : std_logic;
  signal vbunrxkvy : real_vector(2 to 0);
  signal mqyrnftyr : severity_level;
  signal llednvkcr : std_logic;
  signal a : real_vector(2 to 0);
  signal fkfnnw : std_logic_vector(2 to 3);
  signal qbevrjz : severity_level;
  signal dluct : real_vector(2 to 0);
  signal dpl : std_logic_vector(2 to 3);
  signal kbw : severity_level;
  signal lvukj : std_logic;
  signal otwp : real_vector(2 to 0);
  signal cvirtys : std_logic_vector(2 to 3);
  signal ilosdaemi : severity_level;
begin
  ctaei : entity work.hqpynxo
    port map (ahx => ilosdaemi, batsibh => cvirtys, watf => otwp, mxrka => lvukj);
  rt : entity work.hqpynxo
    port map (ahx => kbw, batsibh => dpl, watf => dluct, mxrka => lvukj);
  dmspxu : entity work.hqpynxo
    port map (ahx => qbevrjz, batsibh => fkfnnw, watf => a, mxrka => llednvkcr);
  fqghj : entity work.hqpynxo
    port map (ahx => mqyrnftyr, batsibh => cvirtys, watf => vbunrxkvy, mxrka => vkqbjfl);
  
  -- Multi-driven assignments
  fkfnnw <= ('X', 'X');
  lvukj <= 'X';
  dzjrdbdge <= "";
  fkfnnw <= cvirtys;
end gf;



-- Seed after: 8184151275184182328,7198033922882419595
