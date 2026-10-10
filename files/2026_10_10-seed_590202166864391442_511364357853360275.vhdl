-- Seed: 590202166864391442,511364357853360275

library ieee;
use ieee.std_logic_1164.all;

entity spatwm is
  port ( ctycqabb : linkage std_logic_vector(3 downto 1)
  ; uqlnlreszd : inout real_vector(1 to 2)
  ; iebrweqc : linkage time
  ; vmbzd : inout std_logic_vector(2 to 0)
  );
end spatwm;

architecture ucsini of spatwm is
  
begin
  -- Single-driven assignments
  uqlnlreszd <= (0.4_1_3_2, 332.1_3_0_2);
  
  -- Multi-driven assignments
  vmbzd <= vmbzd;
end ucsini;

entity pmresm is
  port (ovkyhwqjvn : linkage bit; ftjqxzdw : buffer severity_level);
end pmresm;

library ieee;
use ieee.std_logic_1164.all;

architecture r of pmresm is
  signal nugaaehbl : std_logic_vector(2 to 0);
  signal iemni : time;
  signal svboxx : real_vector(1 to 2);
  signal nwn : std_logic_vector(3 downto 1);
  signal lpx : std_logic_vector(2 to 0);
  signal wfl : time;
  signal tdrehdtz : real_vector(1 to 2);
  signal vugsm : std_logic_vector(3 downto 1);
begin
  ffysytwjq : entity work.spatwm
    port map (ctycqabb => vugsm, uqlnlreszd => tdrehdtz, iebrweqc => wfl, vmbzd => lpx);
  gai : entity work.spatwm
    port map (ctycqabb => nwn, uqlnlreszd => svboxx, iebrweqc => iemni, vmbzd => nugaaehbl);
  
  -- Single-driven assignments
  ftjqxzdw <= NOTE;
end r;



-- Seed after: 3779377411110060737,511364357853360275
