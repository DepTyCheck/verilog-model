-- Seed: 5979661515369531082,2230106469645304029

entity ltwor is
  port (ihenq : inout character);
end ltwor;

architecture ibr of ltwor is
  
begin
  -- Single-driven assignments
  ihenq <= 'o';
end ibr;

entity ps is
  port (iadhs : out character; jcphajiy : inout bit);
end ps;

architecture cxupll of ps is
  signal paowsm : character;
  signal zanmpdw : character;
  signal kdlngzchiw : character;
begin
  nknzwsrjus : entity work.ltwor
    port map (ihenq => kdlngzchiw);
  xc : entity work.ltwor
    port map (ihenq => iadhs);
  zw : entity work.ltwor
    port map (ihenq => zanmpdw);
  fotpdgf : entity work.ltwor
    port map (ihenq => paowsm);
end cxupll;



-- Seed after: 15909675851857084321,2230106469645304029
