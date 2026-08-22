-- Seed: 17060226919420331901,5805648483995786113

entity d is
  port (gyiaxwisf : buffer bit_vector(4 to 4); glne : inout time; jliqmnl : out bit_vector(1 to 3); ikbuqyr : linkage boolean);
end d;

architecture ywnwzsfnu of d is
  
begin
  -- Single-driven assignments
  jliqmnl <= ('1', '0', '1');
  gyiaxwisf <= (others => '0');
  glne <= 3_0 ns;
end ywnwzsfnu;

entity epuc is
  port (gxfulvj : buffer bit; pngos : buffer character);
end epuc;

architecture rwwv of epuc is
  signal konedzuu : boolean;
  signal zjkwcbfcm : bit_vector(1 to 3);
  signal nwuebennw : time;
  signal jzlfj : bit_vector(4 to 4);
begin
  z : entity work.d
    port map (gyiaxwisf => jzlfj, glne => nwuebennw, jliqmnl => zjkwcbfcm, ikbuqyr => konedzuu);
  
  -- Single-driven assignments
  gxfulvj <= gxfulvj;
  pngos <= 'x';
end rwwv;

entity mj is
  port (rghgblolcz : out severity_level; wdrhfoea : inout integer);
end mj;

architecture f of mj is
  signal r : boolean;
  signal t : bit_vector(1 to 3);
  signal ye : time;
  signal ouyyvd : bit_vector(4 to 4);
  signal rnop : boolean;
  signal wrwcywg : bit_vector(1 to 3);
  signal gvdvce : time;
  signal ananedrt : bit_vector(4 to 4);
begin
  xjtsnrpgt : entity work.d
    port map (gyiaxwisf => ananedrt, glne => gvdvce, jliqmnl => wrwcywg, ikbuqyr => rnop);
  lyhfru : entity work.d
    port map (gyiaxwisf => ouyyvd, glne => ye, jliqmnl => t, ikbuqyr => r);
  
  -- Single-driven assignments
  wdrhfoea <= 8#1_3#;
  rghgblolcz <= rghgblolcz;
end f;



-- Seed after: 10021306530825118922,5805648483995786113
