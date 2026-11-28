-- Seed: 18170710040352929476,15306146742274979323

entity ioa is
  port (osgi : buffer boolean_vector(2 downto 0); fbuerrjx : out bit_vector(2 downto 1));
end ioa;

architecture cao of ioa is
  
begin
  -- Single-driven assignments
  osgi <= (TRUE, TRUE, FALSE);
  fbuerrjx <= fbuerrjx;
end cao;

entity foaqc is
  port (doc : out time; xvtx : linkage integer; vlyyrbnwel : buffer boolean);
end foaqc;

architecture zvsog of foaqc is
  signal bocw : bit_vector(2 downto 1);
  signal aufsaaiws : boolean_vector(2 downto 0);
  signal ttyhwya : bit_vector(2 downto 1);
  signal rtdka : boolean_vector(2 downto 0);
  signal d : bit_vector(2 downto 1);
  signal uigaxyjvp : boolean_vector(2 downto 0);
begin
  krcywfed : entity work.ioa
    port map (osgi => uigaxyjvp, fbuerrjx => d);
  aiqfmovdgv : entity work.ioa
    port map (osgi => rtdka, fbuerrjx => ttyhwya);
  qohebt : entity work.ioa
    port map (osgi => aufsaaiws, fbuerrjx => bocw);
  
  -- Single-driven assignments
  doc <= doc;
end zvsog;

entity fglwmuob is
  port (qk : linkage real; rlcslg : out integer);
end fglwmuob;

architecture vm of fglwmuob is
  signal ydnsb : bit_vector(2 downto 1);
  signal x : boolean_vector(2 downto 0);
begin
  qyobneilio : entity work.ioa
    port map (osgi => x, fbuerrjx => ydnsb);
end vm;

library ieee;
use ieee.std_logic_1164.all;

entity a is
  port (kdglpgawb : in std_logic);
end a;

architecture rzpi of a is
  signal hnxzbtrinx : bit_vector(2 downto 1);
  signal lnhqg : boolean_vector(2 downto 0);
  signal naqspndnr : bit_vector(2 downto 1);
  signal en : boolean_vector(2 downto 0);
  signal dtdrsosslr : integer;
  signal y : real;
begin
  ahcwrikgc : entity work.fglwmuob
    port map (qk => y, rlcslg => dtdrsosslr);
  qpiwdxydvq : entity work.ioa
    port map (osgi => en, fbuerrjx => naqspndnr);
  jxcz : entity work.ioa
    port map (osgi => lnhqg, fbuerrjx => hnxzbtrinx);
end rzpi;



-- Seed after: 2858438103645270732,15306146742274979323
