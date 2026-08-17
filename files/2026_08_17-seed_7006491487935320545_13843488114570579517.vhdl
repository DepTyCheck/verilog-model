-- Seed: 7006491487935320545,13843488114570579517

entity c is
  port (wagzpwvcnr : in integer);
end c;

architecture yfas of c is
  
begin
  
end yfas;

entity vnqfr is
  port (hygihayv : inout integer; vdoarqkxe : buffer time; rszs : inout real; gcjzjylnpw : buffer severity_level);
end vnqfr;

architecture aqvuhk of vnqfr is
  signal fekioxizn : integer;
begin
  jkf : entity work.c
    port map (wagzpwvcnr => hygihayv);
  fv : entity work.c
    port map (wagzpwvcnr => fekioxizn);
  
  -- Single-driven assignments
  gcjzjylnpw <= FAILURE;
  hygihayv <= hygihayv;
  rszs <= 8#2.76#;
  vdoarqkxe <= 16#E2# ns;
end aqvuhk;

entity hvwi is
  port (iuxjzin : in real; nvsd : in bit_vector(2 to 1));
end hvwi;

architecture eto of hvwi is
  signal oz : integer;
  signal hhh : integer;
  signal q : severity_level;
  signal unfh : real;
  signal cwzbt : time;
  signal empjou : integer;
begin
  prdurp : entity work.vnqfr
    port map (hygihayv => empjou, vdoarqkxe => cwzbt, rszs => unfh, gcjzjylnpw => q);
  llrfgdsslv : entity work.c
    port map (wagzpwvcnr => hhh);
  kwsxsq : entity work.c
    port map (wagzpwvcnr => oz);
  evrxyyb : entity work.c
    port map (wagzpwvcnr => oz);
  
  -- Single-driven assignments
  hhh <= 8#1#;
  oz <= empjou;
end eto;

entity ic is
  port (vkclhxbiv : in boolean; mnlbmjf : out bit);
end ic;

architecture xalwht of ic is
  signal kpwubshmgk : integer;
  signal rqhpzeqaw : bit_vector(2 to 1);
  signal bnrphwt : bit_vector(2 to 1);
  signal ftrqfdfx : real;
  signal hakmkrrhlk : integer;
begin
  e : entity work.c
    port map (wagzpwvcnr => hakmkrrhlk);
  dxhazlwuif : entity work.hvwi
    port map (iuxjzin => ftrqfdfx, nvsd => bnrphwt);
  ubr : entity work.hvwi
    port map (iuxjzin => ftrqfdfx, nvsd => rqhpzeqaw);
  jsxhmh : entity work.c
    port map (wagzpwvcnr => kpwubshmgk);
  
  -- Single-driven assignments
  rqhpzeqaw <= (others => '0');
  mnlbmjf <= mnlbmjf;
  kpwubshmgk <= hakmkrrhlk;
  hakmkrrhlk <= 3_4_4;
end xalwht;



-- Seed after: 15329179837594939919,13843488114570579517
