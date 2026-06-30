-- Seed: 18396515307582529662,14629254427735353553

entity bguf is
  port (ocspd : inout time);
end bguf;

architecture medzfur of bguf is
  
begin
  -- Single-driven assignments
  ocspd <= 8#11055# ps;
end medzfur;

entity nvrrsbzk is
  port (lthwajop : inout severity_level; scokudkrd : out integer; vjtjbiai : inout integer);
end nvrrsbzk;

architecture zdkg of nvrrsbzk is
  
begin
  -- Single-driven assignments
  vjtjbiai <= 02;
  lthwajop <= FAILURE;
  scokudkrd <= 2#1_0#;
end zdkg;

entity bxcvhojyxi is
  port (imerhdtqrv : inout integer);
end bxcvhojyxi;

architecture spxl of bxcvhojyxi is
  signal okb : time;
begin
  rinaybzk : entity work.bguf
    port map (ocspd => okb);
  
  -- Single-driven assignments
  imerhdtqrv <= 2#0_1_0_1#;
end spxl;



-- Seed after: 14628724932777278081,14629254427735353553
