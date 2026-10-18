-- Seed: 3420064567894234256,14108379317686958507

entity ouzulld is
  port (zewnaoivxo : inout integer; eoc : inout real);
end ouzulld;

architecture cswzr of ouzulld is
  
begin
  -- Single-driven assignments
  eoc <= eoc;
  zewnaoivxo <= 424;
end cswzr;

entity v is
  port (dttjid : in time; kuvu : in bit);
end v;

architecture ijejky of v is
  signal uya : real;
  signal azemfq : integer;
begin
  q : entity work.ouzulld
    port map (zewnaoivxo => azemfq, eoc => uya);
end ijejky;

entity hxmrn is
  port (kbl : inout time; kpeneaykqw : inout time_vector(0 downto 3); vnl : out real);
end hxmrn;

architecture zshkkg of hxmrn is
  signal uksv : integer;
  signal fry : real;
  signal yoolpvc : integer;
  signal rwqmllpc : real;
  signal dnhbengq : integer;
  signal gg : bit;
begin
  a : entity work.v
    port map (dttjid => kbl, kuvu => gg);
  saddauozee : entity work.ouzulld
    port map (zewnaoivxo => dnhbengq, eoc => rwqmllpc);
  zjdtgko : entity work.ouzulld
    port map (zewnaoivxo => yoolpvc, eoc => fry);
  m : entity work.ouzulld
    port map (zewnaoivxo => uksv, eoc => vnl);
  
  -- Single-driven assignments
  gg <= '0';
end zshkkg;



-- Seed after: 7277740672874895237,14108379317686958507
